{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
module Handler.Profile where

import Prelude hiding (zip)

import BISocie
import Settings (entryStartYear, graduateStartYear)
import StaticFiles
import Handler.S3

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
    Just _   {-  default  -} -> viewProf
    Nothing  {-  default  -} -> viewProf
  where
    getLab user = 
      if not $ isTeacher user
      then return Nothing
      else do
        ml <- getBy $ UniqueLaboratory uid
        case ml of
          Just (_, l) -> return $ Just l
          Nothing -> return $ Just $ Laboratory { laboratoryHeadResearcher=uid
                                                , laboratoryExtensionNumber=Nothing
                                                , laboratoryRoomNumber=Nothing
                                                , laboratoryCourses=Nothing
                                                }
    getProf user =
      if not $ isStudent user
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
      let viewprof = (ProfileR uid, [("mode", "v")])
          editprof = (ProfileR uid, [("mode", "e")])
      (user, mprof, mlab) <- 
        runDB $ do
          user <- get404 uid
          mprof <- getProf user
          mlab <- getLab user
          return (user, mprof, mlab)
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
      let viewprof = (ProfileR uid, [("mode", "v")])
          editprof = (ProfileR uid, [("mode", "e")])
          (y,_,_) = toGregorian $ utctDay now
      (user, mprof, mlab, eyears, gyears) <-
        runDB $ do
          user <- get404 uid
          unless (self `canEdit` user) $ 
            lift $ permissionDenied "あなたはこのユーザプロファイルを編集することはできません."
          mprof <- getProf user
          mlab <- getLab user
          let eyears = zipWith (\y1 y2 -> (y1==y2, y1)) [Settings.entryStartYear..y+5] $ 
                       repeat (fromMaybe y (fmap (toInteger.profileEntryYear) mprof))
              gyears = zipWith (\y1 y2 -> (Just y1==y2, y1)) [Settings.graduateStartYear..y+5] $
                       repeat (fromMaybe Nothing (fmap (fmap toInteger.profileGraduateYear) mprof))
          return (user, mprof, mlab, eyears, gyears)
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
  (_, self) <- requireAuth
  user <- runDB $ get404 uid
  unless (self `canEdit` user) $ 
    permissionDenied "あなたはこのユーザプロファイルを編集することはできません."
  case userRole user of
    Student -> putStudentProf
    Teacher -> putTeacherProf
    _       -> putUserProf
  where
    putUserProf = do
      (em, fn, gn) <- 
        runFormPost' $ (,,)
        <$> emailInput "email"
        <*> stringInput "familyName"
        <*> stringInput "givenName"
      runDB $ do
        -- update user
        update uid [UserEmail em, UserFamilyName fn, UserGivenName gn]
      redirectParams RedirectTemporary (ProfileR uid) [("mode", "e")]
      
    putTeacherProf = do
      (em, fn, gn) <- 
        runFormPost' $ (,,)
        <$> emailInput "email"
        <*> stringInput "familyName"
        <*> stringInput "givenName"
      (rn, en, cs) <- 
        runFormPost' $ (,,)
        <$> maybeStringInput "roomnumber"
        <*> maybeStringInput "extensionnumber"
        <*> maybeStringInput "courses"
      runDB $ do
        -- update user
        update uid [UserEmail em, UserFamilyName fn, UserGivenName gn]
        mlab <- getBy $ UniqueLaboratory uid
        case mlab of
          Nothing -> do
            insert $ Laboratory { laboratoryHeadResearcher=uid 
                                , laboratoryRoomNumber=rn
                                , laboratoryExtensionNumber=en
                                , laboratoryCourses=cs
                                }
          Just (lid, _) -> do
            update lid [ LaboratoryRoomNumber rn
                       , LaboratoryExtensionNumber en
                       , LaboratoryCourses cs
                       ]
            return lid
      redirectParams RedirectTemporary (ProfileR uid) [("mode", "e")]
    
    putStudentProf = do
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
  (_, self) <- requireAuth
  r <- getUrlRender
  mfhid <- lookupPostParam "avatar"
  let avatar = fmap read mfhid
  runDB $ do
    user <- get404 uid
    unless (self `canEdit` user) $
      lift $ permissionDenied "あなたはこのユーザのアバターを変更することはできません."
    update uid [UserAvatar avatar]
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [ ("uri", jsonScalar $ r $ AvatarImageR uid)
                          , ("avatar", jsonScalar $ showmaybe $ mfhid)
                          ]
