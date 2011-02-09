{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
module Handler.Profile where

import BISocie
import Settings (hamletFile, cassiusFile, juliusFile, widgetFile)

import Yesod.Form.Jquery
import Control.Monad
import Control.Applicative
import Data.Time
import Data.Maybe (fromMaybe)

getProfileR :: UserId -> Handler RepHtml
getProfileR uid = do
  mode <- lookupGetParam "mode"
  case mode of
    Just "v" {- view prof -} -> viewProf
    Just "e" {- edit prof -} -> editProf
    Nothing  {-  default  -} -> viewProf
  where
--    getProf :: (Control.Monad.IO.Class.MonadIO m, PersistBackend m) =>
--                User -> m (Maybe (Key Profile, Profile))
    getProf user y =
      if (userRole user /= Student) 
      then return Nothing 
      else do
        mp <- getBy $ UniqueProfile uid
        case mp of
          Just _ -> return mp
          Nothing -> do
            now <- liftIO getCurrentTime
            let (y, _, _) = toGregorian $ utctDay now
            insert $ Profile { profileUser=uid
                             , profileBirth=fromGregorian (y-18) 1 1
                             , profileEntryYear=fromInteger y
                             , profileGraduateYear=Nothing
                             , profileBranch=""
                             , profileAddress=""
                             , profileLongitude=Nothing
                             , profileLatitude=Nothing
                             , profileTel=""
                             , profileStation=""
                             , profileHomeAddress=""
                             , profileHomeLongitude=Nothing
                             , profileHomeLatitude=Nothing
                             , profileHomeTel=""
                             , profileDesiredCourse=Nothing
                             , profileDesiredWorkLocation=Nothing
                             , profileEmployment=Nothing
                             }
            getBy $ UniqueProfile uid
      
      
    viewProf :: Handler RepHtml
    viewProf = do
      (selfid, self) <- requireAuth
      runDB $ do
        user <- get404 uid
        now <- liftIO getCurrentTime
        let viewable = self == user || userRole self > userRole user
            editable = self == user || userRole self > userRole user
            viewprof = (ProfileR uid, [("mode", "v")])
            editprof = (ProfileR uid, [("mode", "e")])
            (y,_,_) = toGregorian $ utctDay now
        unless viewable $ 
          lift $ permissionDenied "あなたはこのユーザプロファイルを見ることはできません."
        mprof <- getProf user y
        lift $ defaultLayout $ do
          setTitle $ string "Profile"
          addCassius $(cassiusFile "profile")
          addJulius $(juliusFile "profile")
          addHamlet $(hamletFile "viewProfile")
    
    editProf :: Handler RepHtml
    editProf = do
      (selfid, self) <- requireAuth
      runDB $ do
        user <- get404 uid
        now <- liftIO getCurrentTime
        let viewable = self == user || userRole self > userRole user
            editable = self == user || userRole self > userRole user
            viewprof = (ProfileR uid, [("mode", "v")])
            editprof = (ProfileR uid, [("mode", "e")])
            (y,_,_) = toGregorian $ utctDay now
        unless editable $ 
          lift $ permissionDenied "あなたはこのユーザプロファイルを編集することはできません."
        mprof <- getProf user y
        let eyears = zipWith (\y1 y2 -> (y1==y2, y1)) [2000..y+5] $ 
                     repeat (fromMaybe y (fmap (toInteger.profileEntryYear.snd) mprof))
            gyears = zipWith (\y1 y2 -> (Just y1==y2, y1)) [2000..y+5] $
                     repeat (fromMaybe Nothing (fmap (fmap toInteger.profileGraduateYear.snd) mprof))
        lift $ defaultLayout $ do
          setTitle $ string "Profile"
          addCassius $(cassiusFile "profile")
          addJulius $(juliusFile "profile")
          addHamlet $(hamletFile "editProfile")

postProfileR :: UserId -> Handler RepHtml
postProfileR uid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "update" -> putProfileR uid
    _             -> invalidArgs ["The possible values of '_method' is modify"]

putProfileR :: UserId -> Handler RepHtml
putProfileR uid = do
  (selfid, self) <- requireAuth
  runDB $ do
    -- validate
    user <- get404 uid
    let editable = selfid == uid || userRole self > userRole user
    unless editable $ 
      lift $ permissionDenied "あなたはこのユーザプロファイルを編集することはできません."
      -- update user
    (em, fn, gn) <- 
      lift $ runFormPost' $ (,,)
      <$> emailInput "email"
      <*> stringInput "familyName"
      <*> stringInput "givenName"
    update uid [UserEmail em, UserFamilyName fn, UserGivenName gn]
      -- update profile
    mprof <- getBy $ UniqueProfile uid
    case mprof of
      Nothing -> return ()
      Just (pid, _) -> do
        (bir, ey, gy, br, adr, tel, st, hadr, htel, dc, dwl, emp) <- 
          lift $ runFormPost' $ (,,,,,,,,,,,)
          <$> dayInput "birth"
          <*> intInput "entryYear"
          <*> maybeIntInput "graduateYear"
          <*> stringInput "branch"
          <*> stringInput "address"
          <*> stringInput "tel"
          <*> stringInput "station"
          <*> stringInput "homeAddress"
          <*> stringInput "homeTel"
          <*> maybeStringInput "desiredCourse"
          <*> maybeStringInput "desiredWorkLocation"
          <*> maybeStringInput "employment"
        update pid [ ProfileBirth bir
                   , ProfileEntryYear ey
                   , ProfileGraduateYear gy
                   , ProfileBranch br
                   , ProfileAddress adr
                   , ProfileTel tel
                   , ProfileStation st
                   , ProfileHomeAddress hadr
                   , ProfileHomeTel htel
                   , ProfileDesiredCourse dc
                   , ProfileDesiredWorkLocation dwl
                   , ProfileEmployment emp
                   ]
    lift $ redirectParams RedirectTemporary (ProfileR uid) [("mode", "e")]