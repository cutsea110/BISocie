{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Handler.Root
       ( getRootR
       , getHomeR
       , getChangePasswordR
       , getHumanNetworkR
       , getUserLocationsR
       , getSystemBatchR
       , postSystemBatchR
       , getSendReminderMailR
       ) where

import Import
import BISocie.Helpers.Util
import Control.Arrow ((&&&))
import Control.Monad (forM)
import Codec.Binary.UTF8.String (decodeString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Conduit (($$))
import Data.Conduit.List (consume)
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time (fromGregorian)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Shakespeare.Text (stext)
import Network.Mail.Mime
import Yesod.Auth.Owl (setPassR)

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- BISocie.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler Html
getRootR = redirect . HomeR . entityKey =<< requireAuth

getHomeR :: UserId -> Handler Html
getHomeR uid = do
  u <- requireAuth
  defaultLayout $ do
    setTitleI $ MsgHomeOf $ entityVal u
    $(widgetFile "home")

getChangePasswordR :: Handler Html
getChangePasswordR = do
  defaultLayout $ do
    setTitleI MsgChangePassword
    $(widgetFile "change-pass")
    
getHumanNetworkR :: Handler Html
getHumanNetworkR = do
  u <- requireAuth
  defaultLayout $ do
    setTitleI MsgHumanNetwork
    addScriptRemote "https://maps.google.com/maps/api/js?sensor=false"
    $(widgetFile "humannetwork")

getUserLocationsR :: Handler Value
getUserLocationsR = do
  r <- getUrlRender
  profs <- runDB $ do
    us <- selectList [UserRole ==. Student] []
    profs' <- selectList [ProfileUser <-. (map entityKey us)] []
    forM profs' $ \(Entity _ p) -> do
      let (Just (Entity _ u)) = find (\eu -> profileUser p == entityKey eu) us
      return (u, p)
  returnJson $ object ["locations" .= array (map (go r) profs)]
  where
    go r (u, p) = 
      object [ "uri" .= r (ProfileR $ profileUser p)
             , "name" .= userFullName u
             , "lat" .= profileLatitude p
             , "lng" .= profileLongitude p
             ]

getSystemBatchR :: Handler Html
getSystemBatchR = defaultLayout $ do
    setTitleI MsgSystemBatch
    $(widgetFile "systembatch")

postSystemBatchR :: Handler ()
postSystemBatchR = do
  r <- getMessageRender
  Just fi <- lookupFile "studentscsv"
  lbs <- L.fromChunks <$> (fileSource fi $$ consume)
  let recs = filter (not . T.null) $ T.lines $ T.pack $ decodeString $ L.unpack lbs
      recs' = map (T.splitOn ",") recs
  runDB $ do
    users <- selectList [UserIdent <-. map head recs'] []
    profs <- selectList [ProfileUser <-. map entityKey users] []
    forM recs' $ \rec -> do
      let (ident:rawpass:email:fname:gname:eyear:gyear:_) = rec
          (eyear', gyear') = (Just (readText eyear), Just (readText gyear))
      uid' <- case userExist ident users of
        Nothing ->
          insert $ User { userIdent=ident
                        , userPassword=Nothing
                        , userRole=Student
                        , userFamilyName=fname
                        , userGivenName=gname
                        , userEmail=email
                        , userAvatar=Nothing
                        , userActive=True
                        }
        Just (Entity id' _) -> do
          update id' [ UserPassword  =. Nothing
                     , UserRole =. Student
                     , UserFamilyName =. fname
                     , UserGivenName =. gname
                     , UserEmail =. email
                     , UserActive =. True
                     ]
          return id'
      case profExist uid' profs of
        Nothing -> insert $ defaultProfile { profileUser=uid'
                                           , profileEntryYear=eyear'
                                           , profileGraduateYear=gyear'
                                           }
        Just (Entity pid _) -> return pid
  setPNotify $ PNotify JqueryUI Info "Add Student" $ r MsgImportStudents
  redirect SystemBatchR
  where
    userExist :: Text -> [Entity User] -> Maybe (Entity User)
    userExist = find . (\x y -> x == userIdent (entityVal y))
    profExist :: UserId -> [Entity Profile] -> Maybe (Entity Profile)
    profExist = find . (\x y -> x == profileUser (entityVal y))

getSendReminderMailR :: Year -> Month -> Date -> Handler Html
getSendReminderMailR y m d = do
  let rday = fromGregorian y m d
  (r, r') <- (,) <$> getUrlRender <*> getMessageRender
  runDB $ do
    issues <- selectList [IssueReminderdate ==. Just rday] []
    forM issues $ \issue -> do
      sendReminder r r' $ entityVal issue
  defaultLayout [whamlet|_{MsgSendingReminder}|]

sendReminder r r' issue = do
  let (pid, ino) = (issueProject &&& issueNumber) issue
  prj <- get404 pid
  liftIO . renderSendMail =<< mkMail r' prj issue (r $ IssueR pid ino)

mkMail render prj issue url = do
  bcc <- selectMailAddresses $ issueProject issue
  return Mail { mailFrom = fromEmailAddress
              , mailTo = []
              , mailCc = []
              , mailBcc = bcc
              , mailHeaders =
                [ ("Subject", render (MsgSendReminder issue))
                , (mailXHeader, toPathPiece $ issueProject issue)
                ]
              , mailParts =
                  [[ Part "text/plain; charset=utf-8" QuotedPrintableText Nothing []
                     $ TL.encodeUtf8 textPart
                   , Part "text/html; charset=utf-8" QuotedPrintableText Nothing []
                     $ TL.encodeUtf8 htmlPart
                   ]]
              }
  where
    textPart = [stext|
 #{render MsgProject}: #{projectName prj}
 #{render MsgIssue}: #{issueSubject issue}
 #{render MsgLimitDate}: #{showLimitdatetime issue}

 #{render MsgCloseLimitDateOfThisIssue}

 * #{render MsgNoteOnThisReminderMail}
 #{render MsgIssue} URL: #{url}
|]
    htmlPart = TL.decodeUtf8 $ renderHtml [shamlet|
<p>
  <dl>
    <dt>#{render MsgProject}
    <dd>#{projectName prj}
    <dt>#{render MsgIssue}
    <dd>#{issueSubject issue}
    <dt>#{render MsgLimitDate}
    <dd>#{showLimitdatetime issue}

  #{render MsgCloseLimitDateOfThisIssue}

<p>* #{render MsgNoteOnThisReminderMail}
   #{render MsgIssue} URL: <a href=#{url}>#{url}</a>
|]
