{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import BISocie
import BISocie.Helpers.Util
import BISocie.Helpers.Auth.HashDB (encrypt)
import Settings

import Control.Monad (when, unless, forM)
import Data.List (sortBy, intersperse)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as L
import Codec.Binary.UTF8.String (encodeString, decodeString)


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
  page' <- lookupGetParam "page"
  order' <- lookupGetParam "order"
  let page = max 0 $ fromMaybe 0  $ fmap read $ page'
      order = fromMaybe ProjectUdateDesc $ fmap read $ order'
  liftIO $ putStrLn $ show order
  (allprjs, prjs) <- runDB $ do
    prjs' <- viewableProjects (selfid, self)
    let sorted = sortBy (sorter order) prjs'
    return $ (prjs',
              zip (concat $ repeat ["odd", "even"]::[String]) 
              $ take projectListLimit $ drop (page*projectListLimit) sorted)
  let maxpage = ceiling (fromIntegral (length allprjs) / fromIntegral projectListLimit) - 1
      prevExist = page > 0
      nextExist = page < maxpage
      prevPage = (HomeR uid, [("page", show $ max 0 (page-1)), ("order", show order)])
      nextPage = (HomeR uid, [("page", show $ max 0 (page+1)), ("order", show order)])
      pagenate = intersperse [] $  map (map pageN) $ mkPagenate page maxpage pagenateWidth
      pageN = \n -> (n, (HomeR uid, [("page", show n), ("order", show order)]))
      isCurrent = (==page)
      needPaging = maxpage > 0
      inc = (+1)
      udateAsc  = (HomeR selfid, [("page", show page), ("order", show ProjectUdateAsc)])
      udateDesc = (HomeR selfid, [("page", show page), ("order", show ProjectUdateDesc)])
      cdateAsc  = (HomeR selfid, [("page", show page), ("order", show ProjectCdateAsc)])
      cdateDesc = (HomeR selfid, [("page", show page), ("order", show ProjectCdateDesc)])
      nameAsc   = (HomeR selfid, [("page", show page), ("order", show ProjectNameAsc)])
      nameDesc  = (HomeR selfid, [("page", show page), ("order", show ProjectNameDesc)])
  defaultLayout $ do
    setTitle $ string $ userFullName self ++ " ホーム"
    addCassius $(cassiusFile "home")
    addJulius $(juliusFile "home")
    addHamlet $(hamletFile "home")
  where
    sorter :: Order Project -> (ProjectId, Project) -> (ProjectId, Project) -> Ordering
    sorter ProjectUdateAsc  (_, p) (_, q) = projectUdate p `compare` projectUdate q
    sorter ProjectUdateDesc (_, p) (_, q) = projectUdate q `compare` projectUdate p
    sorter ProjectCdateAsc  (_, p) (_, q) = projectCdate p `compare` projectCdate q
    sorter ProjectCdateDesc (_, p) (_, q) = projectCdate q `compare` projectCdate p
    sorter ProjectNameAsc   (_, p) (_, q) = projectName p  `compare` projectName q
    sorter ProjectNameDesc  (_, p) (_, q) = projectName q  `compare` projectName p
    
getHumanNetworkR :: Handler RepHtml
getHumanNetworkR = do
  (selfid, self) <- requireAuth
  unless (canViewHumannetwork self) $ 
    permissionDenied "あなたはヒューマンエットワークを閲覧することはできません."
  defaultLayout $ do
    setTitle $ string "ヒューマンネットワーク"
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
      jsonMap [ ("uri", jsonScalar $ r $ ProfileR $ profileUser p)
              , ("name", jsonScalar $ userFullName u)
              , ("lat", jsonScalar $ showMaybeDouble $ profileLatitude p)
              , ("lng", jsonScalar $ showMaybeDouble $ profileLongitude p)
              ]

getSystemBatchR :: Handler RepHtml
getSystemBatchR = do
  (_, self) <- requireAuth
  unless (isAdmin self) $
    permissionDenied "あなたはこの機能を利用することはできません."
  defaultLayout $ do
    setTitle $ string "システムバッチ"
    addCassius $(cassiusFile "systembatch")
    addHamlet $(hamletFile "systembatch")

postSystemBatchR :: Handler ()
postSystemBatchR = do
  (_, self) <- requireAuth
  unless (isAdmin self) $
    permissionDenied "あなたはこの機能を利用することはできません."
  Just fi <- lookupFile "studentscsv"
  let recs = filter (not . null) $ lines $ decodeString $ L.unpack $ fileContent fi
  runDB $ do
    users <- selectList [] [] 0 0
    forM recs $ \rec -> do
      let (uid:rawpass:email:fname:gname:_) = splitOn "," rec
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
        Just (id', u) -> do
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
    userExist :: String -> [(UserId, User)] -> Maybe (UserId, User)
    userExist _   [] = Nothing
    userExist uid (u@(_, u'):us) = 
      if uid == userIdent u' then Just u else userExist uid us
