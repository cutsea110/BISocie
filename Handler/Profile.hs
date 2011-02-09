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
    viewProf :: Handler RepHtml
    viewProf = do
      (selfid, self) <- requireAuth
      runDB $ do
        user <- get404 uid
        let viewable = self == user || userRole self > userRole user
            editable = self == user || userRole self > userRole user
            cancreateproject = userRole self >= Teacher
        unless viewable $ 
          lift $ permissionDenied "あなたはこのユーザプロファイルを見ることはできません."
        mprof <- do
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
          addJulius $(juliusFile "profile")
          addHamlet $(hamletFile "viewProfile")
    
    editProf :: Handler RepHtml
    editProf = undefined

postProfileR :: UserId -> Handler RepHtml
postProfileR uid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "modify" -> putProfileR uid
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
    lift $ redirect RedirectTemporary $ ProfileR uid
