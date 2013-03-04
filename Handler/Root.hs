{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Handler.Root where

import Yesod
import Yesod.Auth.Owl (setPassR)
import Control.Applicative ((<$>))
import Control.Monad (unless, forM)
import Data.Conduit (($$))
import Data.Conduit.List (consume)
import Data.List (find)
import qualified Data.ByteString.Lazy.Char8 as L
import Codec.Binary.UTF8.String (decodeString)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Internal (preEscapedText)
import Data.Time (fromGregorian)
import Network.Mail.Mime
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as LE
import Network.Wai (Request(..))
import Network.Socket (getNameInfo)

import Foundation
import BISocie.Helpers.Util
import Settings.StaticFiles
import Settings

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- BISocie.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = redirect . HomeR . entityKey =<< requireAuth

getHomeR :: UserId -> Handler RepHtml
getHomeR uid = do
  u <- requireAuth
  defaultLayout $ do
    setTitleI $ MsgHomeOf $ entityVal u
    $(widgetFile "home")

getChangePasswordR :: Handler RepHtml
getChangePasswordR = do
  defaultLayout $ do
    setTitleI MsgChangePassword
    $(widgetFile "change-pass")
    
getHumanNetworkR :: Handler RepHtml
getHumanNetworkR = do
  u <- requireAuth
  defaultLayout $ do
    setTitleI MsgHumanNetwork
    addScriptRemote "https://maps.google.com/maps/api/js?sensor=false"
    $(widgetFile "humannetwork")

getUserLocationsR :: Handler RepJson
getUserLocationsR = do
  r <- getUrlRender
  profs <- runDB $ do
    us <- selectList [UserRole ==. Student] []
    profs' <- selectList [ProfileUser <-. (map entityKey us)] []
    forM profs' $ \(Entity _ p) -> do
      let (Just (Entity _ u)) = find (\eu -> profileUser p == entityKey eu) us
      return (u, p)
  jsonToRepJson $ object ["locations" .= array (map (go r) profs)]
  where
    go r (u, p) = 
      object [ "uri" .= r (ProfileR $ profileUser p)
             , "name" .= userFullName u
             , "lat" .= profileLatitude p
             , "lng" .= profileLongitude p
             ]

getSystemBatchR :: Handler RepHtml
getSystemBatchR = defaultLayout $ do
    setTitleI MsgSystemBatch
    $(widgetFile "systembatch")

postSystemBatchR :: Handler ()
postSystemBatchR = do
  r <- getMessageRender
  Just fi <- lookupFile "studentscsv"
  lbs <- lift $ L.fromChunks <$> (fileSource fi $$ consume)
  let recs = filter (not . T.null) $ T.lines $ T.pack $ decodeString $ L.unpack lbs
      idents = map (head . T.splitOn ",") recs
  runDB $ do
    users <- selectList [UserIdent <-. idents] []
    profs <- selectList [ProfileUser <-. map entityKey users] []
    forM recs $ \rec -> do
      let (ident:rawpass:email:fname:gname:eyear:gyear:_) = T.splitOn "," rec
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

getSendReminderMailR :: Year -> Month -> Date -> Handler RepHtml
getSendReminderMailR y m d = do
  let rday = fromGregorian y m d
  r <- getUrlRender
  req <- fmap reqWaiRequest getRequest
  let rhost = remoteHost req
  (Just rhostname, _) <- liftIO $ getNameInfo [] True True rhost
  unless (rhostname == "localhost") $
    permissionDenied "あなたはこの機能を利用することはできません."
  runDB $ do
    issues <- selectList [IssueReminderdate ==. Just rday] []
    forM issues $ \(Entity _ issue) -> do
      let pid = issueProject issue
          ino = issueNumber issue
      prj <- get404 pid
      emails <- selectMailAddresses $ issueProject issue
      liftIO $ renderSendMail Mail
        { mailFrom = fromEmailAddress
        , mailTo = []
        , mailCc = []
        , mailBcc = emails
        , mailHeaders =
          [ ("Subject", "【リマインダメール送信】" +++ issueSubject issue)
          , (mailXHeader, toPathPiece $ issueProject issue)
          ]
        , mailParts =
            [[ Part
               { partType = "text/plain; charset=utf-8"
               , partEncoding = None
               , partFilename = Nothing
               , partHeaders = []
               , partContent = LE.encodeUtf8 $ TL.pack $ T.unpack $ T.unlines
                               $ [ "プロジェクト: " +++ projectName prj
                                 , "案件: " +++ issueSubject issue
                                 , "期限: " +++ showLimitdatetime issue
                                 , ""
                                 , "この案件の期限が近づいてきました。"
                                 , ""
                                 , "*このメールに直接返信せずにこちらのページから投稿してください。"
                                 , "イシューURL: " +++ r (IssueR pid ino)
                                 ]
               }
             ]]
        }
  defaultLayout [whamlet|$newline never
Sending reminder mails|]
