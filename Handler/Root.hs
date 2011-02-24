{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import BISocie
import Settings

import Control.Monad (unless, forM)
import Data.List

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
  let page = case page' of
        Nothing -> 0
        Just p -> max (read p) 0
  (all, prjs) <- runDB $ do
    ps <- selectList [ParticipantsUserEq selfid] [] 0 0
    prjs' <- forM ps $ \(id, p) -> do
        let pid = participantsProject p
        Just prj <- get pid
        return (pid, prj)
    let sorted = sortBy (\(_, p) (_, q) -> projectUdate q `compare` projectUdate p) prjs'
    return $ (prjs',
              zip (concat $ repeat ["odd", "even"]::[String]) 
              $ take projectListLimit $ drop (page*projectListLimit) sorted)
  let maxpage = ceiling (fromIntegral (length all) / fromIntegral projectListLimit) - 1
      prevExist = page > 0
      nextExist = page < maxpage
      prevPage = (HomeR uid, [("page", show $ max 0 (page-1))])
      nextPage = (HomeR uid, [("page", show $ max 0 (page+1))])
      pagenate = map (map pageN) $ mkPagenate page maxpage 5
      pageN = \n -> (n+1, (HomeR uid, [("page", show n)]))
  defaultLayout $ do
    setTitle $ string $ userFullName self ++ " ホーム"
    addHamlet $(hamletFile "home")
  where
    

mkPagenate :: Int -> Int -> Int -> [[Int]]
mkPagenate current max width =
  if leftConnected && rightConnected
  then [[ll..rr]]
  else if leftConnected
       then [[ll..cr], [rl..rr]]
       else if rightConnected
            then [[ll..lr],[cl..rr]]
            else [[ll..lr],[cl..cr],[rl..rr]]
  where
    leftConnected = cl-lr<=3
    rightConnected = rl-cr<=3
    ll = 0
    lr = width
    cl = current-width
    cr = current+width
    rl = max-width
    rr = max


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
  (selfid, self) <- requireAuth
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
