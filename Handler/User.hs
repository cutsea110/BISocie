{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.User where

import Yesod
import Control.Monad (unless, forM, mplus, join)
import Control.Applicative ((<$>),(<*>))
import Data.Text (isInfixOf)
import Data.Maybe (isNothing)

import Foundation
import BISocie.Helpers.Util

getUserListR :: Handler RepJson
getUserListR = do
  r <- getUrlRender
  (mn, mey, mt, mstf, ma) <- runInputGet $ (,,,,)
                            <$> iopt textField "name_like"
                            <*> fmap (fmap readText) (iopt textField "entry_year")
                            <*> iopt textField "teacher"
                            <*> iopt textField "staff"
                            <*> iopt textField "admin"
  let roles = toRoles ((Teacher, mt), (Student, mey), (Staff, mstf), (Admin, ma))
      roleWhere = if null roles then [] else [UserRole <-. roles]
  us' <- runDB $ do
    us'' <- selectList ([UserActive ==. True]++roleWhere) []
    forM us'' $ \u@(Entity uid _) -> do
      mp' <- getBy $ UniqueProfile uid
      let ra = AvatarImageR uid
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
             , "entryYear" .= fmap (showEntryYear.entityVal) mp
             , "avatar" .= r ra
             ]
    toRoles (t,ey,s,a) = foldr (mplus.q2r) (q2r ey) [t, s, a]
      where q2r (r, m) = maybe [] (const [r]) m
    mkCond mn mey ((Entity _ u), mp, _) =
         (isNothing mn || fullNameOrIdentMatch)
      && (isNothing mey || entryYearMatch)
         where
           fullNameOrIdentMatch = 
             userFullName u `like` mn || userIdent u `like` mn
           entryYearMatch = 
             not (isStudent u) || join (fmap (profileEntryYear.entityVal) mp) == mey
    like _ Nothing = False
    like str (Just pat) = pat `isInfixOf` str
