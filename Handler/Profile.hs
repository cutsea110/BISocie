{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Handler.Profile where

import Foundation

import Settings (entryStartYear, graduateStartYear)
import Settings.StaticFiles
import Handler.S3

import Prelude hiding (zip)
import Yesod
import Control.Monad
import Control.Applicative
import Data.Time
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (Text)

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
          Just (Entity _ l) -> return $ Just l
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
          Just (Entity _ p) -> return $ Just p
          Nothing -> do
            now <- liftIO getCurrentTime
            let (y, _, _) = toGregorian $ utctDay now
            return $ Just $ Profile { profileUser=uid
                                    , profileBirth=Just $ fromGregorian (y-18) 1 1
                                    , profileEntryYear=Just $ fromInteger y
                                    , profileGraduateYear=Nothing
                                    , profileBranch=Nothing
                                    , profileZip=Nothing
                                    , profileAddress=Nothing
                                    , profileLongitude=Nothing
                                    , profileLatitude=Nothing
                                    , profileTel=Nothing
                                    , profileStation=Nothing
                                    , profileHomeZip=Nothing
                                    , profileHomeAddress=Nothing
                                    , profileHomeLongitude=Nothing
                                    , profileHomeLatitude=Nothing
                                    , profileHomeTel=Nothing
                                    , profileDesiredCourse=Nothing
                                    , profileDesiredWorkLocation=Nothing
                                    , profileEmployment=Nothing
                                    }

      
    viewProf :: Handler RepHtml
    viewProf = do
      (Entity selfid self) <- requireAuth
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
        setTitle "Profile"
        addScriptRemote "http://maps.google.com/maps/api/js?sensor=false"
        $(widgetFile "profile")
        $(widgetFile "viewProfile")
    
    editProf :: Handler RepHtml
    editProf = do
      (Entity selfid self) <- requireAuth
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
                       repeat (fromMaybe y (join (fmap (fmap toInteger.profileEntryYear) mprof)))
              gyears = zipWith (\y1 y2 -> (Just y1==y2, y1)) [Settings.graduateStartYear..y+5] $
                       repeat (fromMaybe Nothing (fmap (fmap toInteger.profileGraduateYear) mprof))
          return (user, mprof, mlab, eyears, gyears)
      defaultLayout $ do
        setTitle "Profile"
        addScriptRemote "http://maps.google.com/maps/api/js?sensor=false"
        $(widgetFile "profile")
        $(widgetFile "editProfile")

postProfileR :: UserId -> Handler RepHtml
postProfileR uid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "update" -> putProfileR uid
    _             -> invalidArgs ["The possible values of '_method' is modify"]

putProfileR :: UserId -> Handler RepHtml
putProfileR uid = do
  (Entity _ self) <- requireAuth
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
        runInputPost $ (,,)
        <$> ireq textField "email"
        <*> ireq textField "familyName"
        <*> ireq textField "givenName"
      runDB $ do
        -- update user
        update uid [UserEmail =. em, UserFamilyName =. fn, UserGivenName =. gn]
      redirect (ProfileR uid, [("mode", "e")] :: [(Text, Text)])
      
    putTeacherProf = do
      (em, fn, gn) <- 
        runInputPost $ (,,)
        <$> ireq textField "email"
        <*> ireq textField "familyName"
        <*> ireq textField "givenName"
      lab <- runInputPost $ Laboratory uid
        <$> iopt textField "roomnumber"
        <*> iopt textField "extensionnumber"
        <*> iopt textField "courses"
      runDB $ do
        -- update user
        update uid [UserEmail =. em, UserFamilyName =. fn, UserGivenName =. gn]
        mlab <- getBy $ UniqueLaboratory uid
        case mlab of
          Nothing -> insert lab
          Just (Entity lid _) -> replace lid lab >> return lid
      redirect (ProfileR uid, [("mode", "e")] :: [(Text, Text)])
    
    putStudentProf = do
      (em, fn, gn) <- 
        runInputPost $ (,,)
        <$> ireq textField "email"
        <*> ireq textField "familyName"
        <*> ireq textField "givenName"
      prof <- runInputPost $ Profile uid
        <$> iopt dayField "birth"
        <*> iopt intField "entryYear"
        <*> iopt intField "graduateYear"
        <*> iopt textField "branch"
        <*> iopt textField "zip"
        <*> iopt textField "address"
        <*> iopt doubleField "longitude"
        <*> iopt doubleField "latitude"
        <*> iopt textField "tel"
        <*> iopt textField "station"
        <*> iopt textField "homeZip"
        <*> iopt textField "homeAddress"
        <*> iopt doubleField "homeLongitude"
        <*> iopt doubleField "homeLatitude"
        <*> iopt textField "homeTel"
        <*> iopt textField "desiredCourse"
        <*> iopt textField "desiredWorkLocation"
        <*> iopt textField "employment"
      runDB $ do
        -- update user
        update uid [UserEmail =. em, UserFamilyName =. fn, UserGivenName =. gn]
        mprof <- getBy $ UniqueProfile uid
        case mprof of
          Nothing -> insert prof
          Just (Entity pid _) -> replace pid prof >> return pid
      redirect (ProfileR uid, [("mode", "e")] :: [(Text, Text)])
    
getAvatarImageR :: UserId -> Handler RepHtml
getAvatarImageR uid = do
  _ <- requireAuth
  (fid, f) <- runDB $ do
    u <- get404 uid
    case userAvatar u of
      Nothing -> lift $ redirect $ StaticR img_no_image_png
      Just fid -> do
        f <- get404 fid
        return (fid, f)
  getFileR (fileHeaderCreator f) fid

postAvatarR :: UserId -> Handler RepJson
postAvatarR uid = do
  (Entity _ self) <- requireAuth
  r <- getUrlRender
  mfhid <- lookupPostParam "avatar"
  let avatar = fmap (fromJust . fromPathPiece) mfhid
  runDB $ do
    user <- get404 uid
    unless (self `canEdit` user) $
      lift $ permissionDenied "あなたはこのユーザのアバターを変更することはできません."
    update uid [UserAvatar =. avatar]
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ object [ "uri" .= r (AvatarImageR uid)
                         , "avatar" .= showmaybe mfhid
                         ]
