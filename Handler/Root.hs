{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Handler.Root where

import Control.Monad (unless, forM)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as L
import Codec.Binary.UTF8.String (decodeString)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze (preEscapedText)
import Text.Hamlet (hamletFile)
import Text.Cassius (cassiusFile)
import Text.Julius (juliusFile)

import Foundation
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
  ordName <- fmap (fromMaybe "DescProjectUdate") $ lookupGetParam "order"
  let order = textToOrder ordName
  (allprjs, prjs) <- runDB $ do
    prjs' <- if isAdmin self
             then selectList [] [order]
             else do
               ps <- selectList [ParticipantsUser ==. selfid] []
               selectList [ProjectId <-. (map (participantsProject . snd) ps)] [order]
    return $ (prjs',
              zip (concat $ repeat ["odd", "even"]::[String]) 
              $ take projectListLimit $ drop (page*projectListLimit) prjs')
  let maxpage = ceiling (fromIntegral (length allprjs) / fromIntegral projectListLimit) - 1
      prevExist = page > 0
      nextExist = page < maxpage
      prevPage = (HomeR uid, [("page", showText $ max 0 (page-1)), ("order", ordName)])
      nextPage = (HomeR uid, [("page", showText $ max 0 (page+1)), ("order", ordName)])
      pagenate = intersperse [] $  map (map pageN) $ mkPagenate fillGapWidth pagenateWidth page maxpage
      pageN = \n -> (n, (HomeR uid, [("page", showText n), ("order", ordName)]))
      isCurrent = (==page)
      needPaging = maxpage > 0
      inc = (+1)
      udateAsc  = (HomeR selfid, [("page", showText page), ("order", "AscProjectUdate")])
      udateDesc = (HomeR selfid, [("page", showText page), ("order", "DescProjectUdate")])
      cdateAsc  = (HomeR selfid, [("page", showText page), ("order", "AscProjectCdate")])
      cdateDesc = (HomeR selfid, [("page", showText page), ("order", "DescProjectCdate")])
      nameAsc   = (HomeR selfid, [("page", showText page), ("order", "AscProjectName")])
      nameDesc  = (HomeR selfid, [("page", showText page), ("order", "DescProjectName")])
      colspan = 4
      paging = $(hamletFile "hamlet/paging.hamlet")
  defaultLayout $ do
    setTitle $ preEscapedText $ userFullName self +++ " ホーム"
    addCassius $(cassiusFile "cassius/home.cassius")
    addJulius $(juliusFile "julius/home.julius")
    addHamlet $(hamletFile "hamlet/home.hamlet")
    
getHumanNetworkR :: Handler RepHtml
getHumanNetworkR = do
  (selfid, self) <- requireAuth
  unless (canViewHumannetwork self) $ 
    permissionDenied "あなたはヒューマンエットワークを閲覧することはできません."
  defaultLayout $ do
    setTitle "ヒューマンネットワーク"
    addCassius $(cassiusFile "cassius/humannetwork.cassius")
    addJulius $(juliusFile "julius/humannetwork.julius")
    addScriptRemote "http://maps.google.com/maps/api/js?sensor=false"
    addHamlet $(hamletFile "hamlet/humannetwork.hamlet")

getUserLocationsR :: Handler RepJson
getUserLocationsR = do
  (_, self) <- requireAuth
  r <- getUrlRender
  unless (canViewUserLocations self) $ 
    permissionDenied "あなたはこの情報を取得することはできません."
  profs <- runDB $ do
    us <- selectList [UserRole ==. Student] []
    profs' <- selectList [ProfileUser <-. (map fst us)] []
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
    addCassius $(cassiusFile "cassius/systembatch.cassius")
    addHamlet $(hamletFile "hamlet/systembatch.hamlet")

postSystemBatchR :: Handler ()
postSystemBatchR = do
  (_, self) <- requireAuth
  unless (isAdmin self) $
    permissionDenied "あなたはこの機能を利用することはできません."
  Just fi <- lookupFile "studentscsv"
  let recs = filter (not . T.null) $ T.lines $ T.pack $ decodeString $ L.unpack $ fileContent fi
  runDB $ do
    users <- selectList [] []
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
          update id' [ UserPassword  =. Just (encrypt rawpass)
                     , UserRole =. Student
                     , UserFamilyName =. fname
                     , UserGivenName =. gname
                     , UserEmail =. email
                     , UserActive =. True
                     ]
          return id'
  setMessage "学生を登録しました。"
  redirect RedirectTemporary SystemBatchR
  where
    userExist :: Text -> [(UserId, User)] -> Maybe (UserId, User)
    userExist _   [] = Nothing
    userExist uid (u@(_, u'):us) = 
      if uid == userIdent u' then Just u else userExist uid us
