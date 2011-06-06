{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Control.Monad (unless, forM)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as L
import Codec.Binary.UTF8.String (decodeString)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Hamlet (preEscapedText)

import BISocie
import BISocie.Helpers.Util
import BISocie.Helpers.Auth.HashDB (encrypt)
import Settings

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- BISocie.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    (uid, _) <- requireAuth
    redirect RedirectTemporary $ HomeR uid

getHomeR :: UserId -> Handler RepHtml
getHomeR uid = do
  (selfid, self) <- requireAuth
  unless (selfid==uid) $ permissionDenied "他人のホームを見ることはできません."
  page <- fmap (max 0 . fromMaybe 0 . fmap (read . T.unpack)) $ lookupGetParam "page"
  order <- fmap (fromMaybe ProjectUdateDesc . fmap (read . T.unpack)) $ lookupGetParam "order"
  liftIO $ putStrLn $ show order
  (allprjs, prjs) <- runDB $ do
    prjs' <- if isAdmin self
             then selectList [] [order] 0 0
             else do
               ps <- selectList [ParticipantsUserEq selfid] [] 0 0
               selectList [ProjectIdIn (map (participantsProject . snd) ps)] [order] 0 0
    return $ (prjs',
              zip (concat $ repeat ["odd", "even"]::[String]) 
              $ take projectListLimit $ drop (page*projectListLimit) prjs')
  let maxpage = ceiling (fromIntegral (length allprjs) / fromIntegral projectListLimit) - 1
      prevExist = page > 0
      nextExist = page < maxpage
      prevPage = (HomeR uid, [("page", showText $ max 0 (page-1)), ("order", showText order)])
      nextPage = (HomeR uid, [("page", showText $ max 0 (page+1)), ("order", showText order)])
      pagenate = intersperse [] $  map (map pageN) $ mkPagenate page maxpage pagenateWidth
      pageN = \n -> (n, (HomeR uid, [("page", showText n), ("order", showText order)]))
      isCurrent = (==page)
      needPaging = maxpage > 0
      inc = (+1)
      udateAsc  = (HomeR selfid, [("page", showText page), ("order", showText ProjectUdateAsc)])
      udateDesc = (HomeR selfid, [("page", showText page), ("order", showText ProjectUdateDesc)])
      cdateAsc  = (HomeR selfid, [("page", showText page), ("order", showText ProjectCdateAsc)])
      cdateDesc = (HomeR selfid, [("page", showText page), ("order", showText ProjectCdateDesc)])
      nameAsc   = (HomeR selfid, [("page", showText page), ("order", showText ProjectNameAsc)])
      nameDesc  = (HomeR selfid, [("page", showText page), ("order", showText ProjectNameDesc)])
      colspan = 4
      paging = $(hamletFile "paging")
  defaultLayout $ do
    setTitle $ preEscapedText $ userFullName self +++ " ホーム"
    addCassius $(cassiusFile "home")
    addJulius $(juliusFile "home")
    addHamlet $(hamletFile "home")
    
getHumanNetworkR :: Handler RepHtml
getHumanNetworkR = do
  (selfid, self) <- requireAuth
  unless (canViewHumannetwork self) $ 
    permissionDenied "あなたはヒューマンエットワークを閲覧することはできません."
  defaultLayout $ do
    setTitle "ヒューマンネットワーク"
    addCassius $(cassiusFile "humannetwork")
    addJulius $(juliusFile "humannetwork")
    addScriptRemote "http://maps.google.com/maps/api/js?sensor=false"
    addHamlet $(hamletFile "humannetwork")

getUserLocationsR :: Handler RepJson
getUserLocationsR = do
  (_, self) <- requireAuth
  r <- getUrlRender
  unless (canViewUserLocations self) $ 
    permissionDenied "あなたはこの情報を取得することはできません."
  profs <- runDB $ do
    us <- selectList [UserRoleEq Student] [] 0 0
    profs' <- selectList [ProfileUserIn $ map fst us] [] 0 0
    forM profs' $ \(_, p) -> do
      let (Just u) = lookup (profileUser p) us
      return (u, p)
  jsonToRepJson $ jsonMap [("locations", jsonList $ map (go r) profs)]
  where
    go r (u, p) = 
      jsonMap [ ("uri", jsonScalar $ T.unpack $ r $ ProfileR $ profileUser p)
              , ("name", jsonScalar $ T.unpack $ userFullName u)
              , ("lat", jsonScalar $ T.unpack $ showMaybeDouble $ profileLatitude p)
              , ("lng", jsonScalar $ T.unpack $ showMaybeDouble $ profileLongitude p)
              ]

getSystemBatchR :: Handler RepHtml
getSystemBatchR = do
  (_, self) <- requireAuth
  unless (isAdmin self) $
    permissionDenied "あなたはこの機能を利用することはできません."
  defaultLayout $ do
    setTitle "システムバッチ"
    addCassius $(cassiusFile "systembatch")
    addHamlet $(hamletFile "systembatch")

postSystemBatchR :: Handler ()
postSystemBatchR = do
  (_, self) <- requireAuth
  unless (isAdmin self) $
    permissionDenied "あなたはこの機能を利用することはできません."
  Just fi <- lookupFile "studentscsv"
  let recs = filter (not . T.null) $ T.lines $ T.pack $ decodeString $ L.unpack $ fileContent fi
  runDB $ do
    users <- selectList [] [] 0 0
    forM recs $ \rec -> do
      let (uid:rawpass:email:fname:gname:_) = T.splitOn "," rec
      case userExist uid users of
        Nothing ->
          insert $ User { userIdent=uid
                        , userPassword=Just (encrypt rawpass)
                        , userRole=Student
                        , userFamilyName=fname
                        , userGivenName=gname
                        , userEmail=email
                        , userAvatar=Nothing
                        , userActive=True
                        }
        Just (id', _) -> do
          update id' [ UserPassword  $ Just (encrypt rawpass)
                     , UserRole Student
                     , UserFamilyName fname
                     , UserGivenName gname
                     , UserEmail email
                     , UserActive True
                     ]
          return id'
  setMessage "学生を登録しました。"
  redirect RedirectTemporary SystemBatchR
  where
    userExist :: Text -> [(UserId, User)] -> Maybe (UserId, User)
    userExist _   [] = Nothing
    userExist uid (u@(_, u'):us) = 
      if uid == userIdent u' then Just u else userExist uid us
