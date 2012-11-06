{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.User where

import Yesod
import Control.Monad (unless, forM)
import Control.Applicative ((<$>),(<*>))
import Control.Monad (join)
import Data.Text (isInfixOf)
import Data.Maybe (isNothing)

import Foundation
import BISocie.Helpers.Util

getUserListR :: Handler RepJson
getUserListR = do
  (Entity _ self) <- requireAuth
  r <- getUrlRender
  unless (canSearchUser self) $ 
    permissionDenied "あなたは他のユーザを検索することはできません."
  (mn, mey, mt, mstf, mstd, ma) <- runInputGet $ (,,,,,)
                            <$> iopt textField "name_like"
                            <*> fmap (fmap readText) (iopt textField "entry_year")
                            <*> iopt textField "teacher"
                            <*> iopt textField "staff"
                            <*> iopt textField "student"
                            <*> iopt textField "admin"
  let tch = maybe [] (const [Teacher]) mt
      std = maybe [] (const [Student]) mey
      stf = maybe [] (const [Staff]) mstf
      adm = maybe [] (const [Admin]) ma
      roles = tch++std++stf++adm
      roleWhere = if null roles then [] else [UserRole <-. roles]
  us' <- runDB $ do
    us'' <- selectList ([UserActive ==. True]++roleWhere) []
    forM us'' $ \u@(Entity uid _) -> do
      mp' <- getBy $ UniqueProfile uid
      let ra = AvatarImageR uid
      liftIO $ putStrLn $ show $ join (fmap (profileEntryYear.entityVal) mp')==mey
      return (u, mp', ra)
  let us = filter (mkCond mn mey) us'
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
    mkCond mn mey ((Entity _ u), mp, _) =
         (isNothing mn || userFullName u `like` mn || userIdent u `like` mn)
      && (isNothing mey || not (isStudent u) || join (fmap (profileEntryYear.entityVal) mp) == mey)
    like _ Nothing = False
    like str (Just pat) = pat `isInfixOf` str
