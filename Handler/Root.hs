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
  (uid', u) <- requireAuth
  when (uid'/=uid) $ permissionDenied "You couldn't access another users home."
  ps <- runDB $ selectList [ParticipantsUserEq uid] [] 0 0
  defaultLayout $ do
    setTitle $ string $ "Home " ++ userDisplayName u
    addHamlet $(hamletFile "home")
