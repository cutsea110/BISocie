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

profileFormlet :: UserId -> Formlet s BISocie Profile
profileFormlet uid mp = fieldsToTable $ Profile
                    <$> pure (fromMaybe uid (fmap profileUser mp))
                    <*> jqueryDayField def "生年月日" (fmap profileBirth mp)
                    <*> intField "入学年度" (fmap profileEntryYear mp)
                    <*> maybeIntField "卒業年度" (fmap profileGraduateYear mp)
                    <*> stringField "ブランチ" (fmap profileBranch mp)
                    <*> stringField "現住所" (fmap profileAddress mp)
                    <*> maybeDoubleField "現緯度" (fmap profileLongitude mp)
                    <*> maybeDoubleField "現経度" (fmap profileLatitude mp)
                    <*> stringField "電話番号" (fmap profileTel mp)
                    <*> stringField "最寄駅" (fmap profileStation mp)
                    <*> stringField "実家住所" (fmap profileHomeAddress mp)
                    <*> maybeDoubleField "実家緯度" (fmap profileHomeLongitude mp)
                    <*> maybeDoubleField "実家経度" (fmap profileHomeLatitude mp)
                    <*> stringField "実家電話番号" (fmap profileHomeTel mp)
                    <*> maybeStringField "希望進路" (fmap profileDesiredCourse mp)
                    <*> maybeStringField "希望勤務地" (fmap profileDesiredWorkLocation mp)
                    <*> maybeStringField "勤務(予定)先" (fmap profileEmployment mp)


getProfileR :: UserId -> Handler RepHtml
getProfileR uid = do
  (selfid, self) <- requireAuth
  runDB $ do
    user <- get404 uid
    let viewable = self == user || userRole self > userRole user
        editable = self == user || userRole self > userRole user
        cancreateproject = userRole self >= Teacher
    unless viewable $ 
      lift $ permissionDenied "あなたはこのユーザプロファイルを見ることはできません."
    mprof <-
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
    lift $ defaultLayout $ do
      setTitle $ string "Profile"
      addCassius $(cassiusFile "profile")
      addHamlet $(hamletFile "profile")

postProfileR :: UserId -> Handler RepHtml
postProfileR uid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "modify" -> putProfileR uid
    _             ->invalidArgs ["The possible values of '_method' is modify"]

putProfileR :: UserId -> Handler RepHtml
putProfileR uid = do
  (selfid, self) <- requireAuth
  runDB $ do
    -- validate
    user <- get404 uid
    let editable = selfid == uid || userRole self > userRole user
    unless editable $ 
      lift $ permissionDenied "あなたはこのユーザプロファイルを編集することはできません."
    lift $ redirect RedirectTemporary $ ProfileR uid
