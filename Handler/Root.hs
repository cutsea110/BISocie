{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Root where

import BISocie
import Control.Monad (when, forM)

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
  when (selfid/=uid) $ permissionDenied "他人のホームを見ることはできません."
  let cancreateproject = userRole self >= Teacher
  runDB $ do
    ps <- selectList [ParticipantsUserEq selfid] [] 0 0
    prjs <- forM ps $ \(id, p) -> do
      let pid = participantsProject p
      Just prj <- get pid
      return (pid, prj)
    lift $ defaultLayout $ do
      setTitle $ string $ userDisplayName self ++ " ホーム"
      addHamlet $(hamletFile "home")
