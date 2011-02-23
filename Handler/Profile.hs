{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
module Handler.Profile where

import BISocie
import Settings (hamletFile, cassiusFile, juliusFile, widgetFile, entryStartYear, graduateStartYear)
import StaticFiles
import Handler.S3

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
-- |  getProf :: (Control.Monad.IO.Class.MonadIO m, PersistBackend m) =>
--                User -> m (Maybe (Key Profile, Profile))
    getProf user y =
      if (userRole user /= Student) 
      then return Nothing 
      else do
        mp <- getBy $ UniqueProfile uid
        case mp of
          Just (_, p) -> return $ Just p
          Nothing -> do
            now <- liftIO getCurrentTime
            let (y, _, _) = toGregorian $ utctDay now
            return $ Just $ Profile { profileUser=uid
                                    , profileBirth=fromGregorian (y-18) 1 1
                                    , profileEntryYear=fromInteger y
                                    , profileGraduateYear=Nothing
                                    , profileBranch=""
                                    , profileZip=""
                                    , profileAddress=""
                                    , profileLongitude=Nothing
                                    , profileLatitude=Nothing
                                    , profileTel=""
                                    , profileStation=""
                                    , profileHomeZip=""
                                    , profileHomeAddress=""
                                    , profileHomeLongitude=Nothing
                                    , profileHomeLatitude=Nothing
                                    , profileHomeTel=""
                                    , profileDesiredCourse=Nothing
                                    , profileDesiredWorkLocation=Nothing
                                    , profileEmployment=Nothing
                                    }

      
    viewProf :: Handler RepHtml
    viewProf = do
      (selfid, self) <- requireAuth
      now <- liftIO getCurrentTime
      (user, viewable, editable, viewableTel, viewprof, editprof, mprof) <- 
        runDB $ do
          user <- get404 uid
          let viewable = self == user || userRole self > userRole user
              editable = self == user || userRole self > userRole user
              viewableTel = self == user || userRole self >= Staff
              viewprof = (ProfileR uid, [("mode", "v")])
              editprof = (ProfileR uid, [("mode", "e")])
              (y,_,_) = toGregorian $ utctDay now
          mprof <- getProf user y
          return (user, viewable, editable, viewableTel, viewprof, editprof, mprof)
      defaultLayout $ do
        setTitle $ string "Profile"
        addCassius $(cassiusFile "profile")
        addScriptRemote "http://maps.google.com/maps/api/js?sensor=false"
        addJulius $(juliusFile "profile")
        addHamlet $(hamletFile "viewProfile")
    
    editProf :: Handler RepHtml
    editProf = do
      (selfid, self) <- requireAuth
      now <- liftIO getCurrentTime
      
      (user, viewable, editable, editableTel, viewprof, editprof, mprof, eyears, gyears) <-
        runDB $ do
          user <- get404 uid
          let viewable = self == user || userRole self > userRole user
              editable = self == user || userRole self > userRole user
              editableTel = self == user || userRole self >= Staff
              viewprof = (ProfileR uid, [("mode", "v")])
              editprof = (ProfileR uid, [("mode", "e")])
              (y,_,_) = toGregorian $ utctDay now
          unless editable $ 
            lift $ permissionDenied "あなたはこのユーザプロファイルを編集することはできません."
          mprof <- getProf user y
          let eyears = zipWith (\y1 y2 -> (y1==y2, y1)) [Settings.entryStartYear..y+5] $ 
                       repeat (fromMaybe y (fmap (toInteger.profileEntryYear) mprof))
              gyears = zipWith (\y1 y2 -> (Just y1==y2, y1)) [Settings.graduateStartYear..y+5] $
                       repeat (fromMaybe Nothing (fmap (fmap toInteger.profileGraduateYear) mprof))
          return (user, viewable, editable, editableTel, viewprof, editprof, mprof, eyears, gyears)
      defaultLayout $ do
        setTitle $ string "Profile"
        addCassius $(cassiusFile "profile")
        addScriptRemote "http://maps.google.com/maps/api/js?sensor=false"
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
  user <- runDB $ get404 uid
  let editable = selfid == uid || userRole self > userRole user
  unless editable $ 
    permissionDenied "あなたはこのユーザプロファイルを編集することはできません."
  case userRole user of
    Student -> putStudentProf user
    _       -> putTeacher user
  where
    putTeacher user = do
      (em, fn, gn) <- 
        runFormPost' $ (,,)
        <$> emailInput "email"
        <*> stringInput "familyName"
        <*> stringInput "givenName"
      runDB $ do
        -- update user
        update uid [UserEmail em, UserFamilyName fn, UserGivenName gn]
      redirectParams RedirectTemporary (ProfileR uid) [("mode", "e")]
      
    putStudentProf user = do
      (em, fn, gn) <- 
        runFormPost' $ (,,)
        <$> emailInput "email"
        <*> stringInput "familyName"
        <*> stringInput "givenName"
      (bir, ey, gy, br, zip, adr, lon, lat, tel, st, hzip, hadr, hlon, hlat, htel, dc, dwl, emp) <- 
        runFormPost' $ (,,,,,,,,,,,,,,,,,)
        <$> dayInput "birth"
        <*> intInput "entryYear"
        <*> maybeIntInput "graduateYear"
        <*> stringInput "branch"
        <*> stringInput "zip"
        <*> stringInput "address"
        <*> maybeStringInput "longitude"
        <*> maybeStringInput "latitude"
        <*> stringInput "tel"
        <*> stringInput "station"
        <*> stringInput "homeZip"
        <*> stringInput "homeAddress"
        <*> maybeStringInput "homeLongitude"
        <*> maybeStringInput "homeLatitude"
        <*> stringInput "homeTel"
        <*> maybeStringInput "desiredCourse"
        <*> maybeStringInput "desiredWorkLocation"
        <*> maybeStringInput "employment"
      let lon' = fromMaybe Nothing (fmap (Just . read) lon)
          lat' = fromMaybe Nothing (fmap (Just . read) lat)
          hlon' = fromMaybe Nothing (fmap (Just . read) hlon)
          hlat' = fromMaybe Nothing (fmap (Just . read) hlat)
      runDB $ do
        -- update user
        update uid [UserEmail em, UserFamilyName fn, UserGivenName gn]
        mprof <- getBy $ UniqueProfile uid
        case mprof of
          Nothing -> do
            insert $ Profile { profileUser=uid
                             , profileBirth=bir
                             , profileEntryYear=ey
                             , profileGraduateYear=gy
                             , profileBranch=br
                             , profileZip=zip
                             , profileAddress=adr
                             , profileLongitude=lon'
                             , profileLatitude=lat'
                             , profileTel=tel
                             , profileStation=st
                             , profileHomeZip=hzip
                             , profileHomeAddress=hadr
                             , profileHomeLongitude=hlon'
                             , profileHomeLatitude=hlat'
                             , profileHomeTel=htel
                             , profileDesiredCourse=dc
                             , profileDesiredWorkLocation=dwl
                             , profileEmployment=emp
                             }
          Just (pid, _) -> do
            update pid [ ProfileBirth bir
                       , ProfileEntryYear ey
                       , ProfileGraduateYear gy
                       , ProfileBranch br
                       , ProfileZip zip
                       , ProfileAddress adr
                       , ProfileLongitude lon'
                       , ProfileLatitude lat'
                       , ProfileTel tel
                       , ProfileStation st
                       , ProfileHomeZip hzip
                       , ProfileHomeAddress hadr
                       , ProfileHomeLongitude hlon'
                       , ProfileHomeLatitude hlat'
                       , ProfileHomeTel htel
                       , ProfileDesiredCourse dc
                       , ProfileDesiredWorkLocation dwl
                       , ProfileEmployment emp
                       ]
            return pid
      redirectParams RedirectTemporary (ProfileR uid) [("mode", "e")]
    
getAvatarImageR :: UserId -> Handler RepHtml
getAvatarImageR uid = do
  _ <- requireAuth
  (fid, f) <- runDB $ do
    u <- get404 uid
    case userAvatar u of
      Nothing -> lift $ redirect RedirectTemporary $ StaticR img_no_image_png
      Just fid -> do
        f <- get404 fid
        return (fid, f)
  getFileR (fileHeaderCreator f) fid

postAvatarR :: UserId -> Handler RepJson
postAvatarR uid = do
  (selfid, self) <- requireAuth
  r <- getUrlRender
  mfhid <- lookupPostParam "avatar"
  let avatar = fmap read mfhid
  runDB $ do
    user <- get404 uid
    let editable = self == user || userRole self > userRole user
    unless editable $
      lift $ permissionDenied "あなたはこのユーザのアバターを変更することはできません."
    update uid [UserAvatar avatar]
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [ ("uri", jsonScalar $ r $ AvatarImageR uid)
                          , ("avatar", jsonScalar $ showmaybe $ mfhid)
                          ]
