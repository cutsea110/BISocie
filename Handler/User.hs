{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.User where

import Yesod
import Control.Monad (unless, forM)
import qualified Data.Text as T

import Foundation

getUserListR :: Handler RepJson
getUserListR = do
  (Entity _ self) <- requireAuth
  r <- getUrlRender
  unless (canSearchUser self) $ 
    permissionDenied "あなたは他のユーザを検索することはできません."
  us <- runDB $ do
    us' <- selectList [UserActive ==. True] []
    forM us' $ \u@(Entity uid _) -> do
      mp' <- getBy $ UniqueProfile uid
      let ra = AvatarImageR uid
      return (u, mp', ra)
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ object ["userlist" .= array (map (go r) us)]
  where
    go r ((Entity uid u), mp, ra) = 
      object [ "id" .= show uid
             , "ident" .= userIdent u
             , "uri" .= r (ProfileR uid)
             , "name" .= userFullName u
             , "role" .= show (userRole u)
             , "prettyrole" .= userRoleName u
             , "entryYear" .= showmaybe (fmap (showEntryYear.entityVal) mp)
             , "avatar" .= r ra
             ]
