{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import BISocie
import Control.Monad (unless, forM)

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
  let cancreateproject = userRole self >= Teacher
      viewablehumannet = userRole self >= Teacher
  prjs <- runDB $ do
    ps <- selectList [ParticipantsUserEq selfid] [] 0 0
    prjs' <- forM ps $ \(id, p) -> do
        let pid = participantsProject p
        Just prj <- get pid
        return (pid, prj)
    return $ zip (concat $ repeat ["odd", "even"]::[String]) prjs'
  defaultLayout $ do
    setTitle $ string $ userFullName self ++ " ホーム"
    addHamlet $(hamletFile "home")

getHumanNetworkR :: Handler RepHtml
getHumanNetworkR = do
  (selfid, self) <- requireAuth
  let cancreateproject = userRole self >= Teacher
      viewablehumannet = userRole self >= Teacher
  unless viewablehumannet $ 
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
  let viewable = userRole self >= Teacher
  unless viewable $ permissionDenied "あなたはこの情報を取得することはできません."
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
