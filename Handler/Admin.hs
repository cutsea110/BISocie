{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Admin where

import Import
import BISocie.Helpers.Util
import Control.Monad (unless)
import qualified Data.Text as T

getUsersR :: Handler RepHtml
getUsersR = do
  users <- runDB $ selectList [] [Asc UserIdent]
  defaultLayout $(whamletFile "templates/users.hamlet")

getUserR :: UserId -> Handler RepHtml
getUserR uid = do
  user <- runDB $ get404 uid
  let roleIs r = r == userRole user
      toInt = (+1) . fromEnum
  defaultLayout $(whamletFile "templates/user.hamlet")
  where
    roles = [(T.pack $ show r, r) | r <- [minBound::Role .. maxBound]]
    

postUserR :: UserId -> Handler ()
postUserR uid = do
  new <- runInputPost $ User
         <$> ireq textField "ident"
         <*> iopt passwordField "password"
         <*> ireq (selectFieldList roles) "role"
         <*> ireq textField "familyName"
         <*> ireq textField "givenName"
         <*> ireq textField "email"
         <*> fmap (fmap readText) (iopt textField "avatar") -- always Nothing
         <*> ireq boolField "active"
  runDB $ do 
    orig <- get404 uid
    replace uid new { userAvatar = userAvatar orig }
  redirect $ UserR uid
  where
    roles = [(T.pack $ show r, r) | r <- [minBound::Role .. maxBound]]

getNewUserR :: Handler RepHtml
getNewUserR = do
  let toInt = (+1) . fromEnum
  defaultLayout $(whamletFile "templates/newUser.hamlet")
  where
    roles = [(T.pack $ show r, r) | r <- [minBound::Role .. maxBound]]

postNewUserR :: Handler ()
postNewUserR = do
  (Entity _ self) <- requireAuth
  unless (isAdmin self) $
    permissionDenied "あなたはこのページを参照できません."
  new <- runInputPost $ User
         <$> ireq textField "ident"
         <*> iopt passwordField "password"
         <*> ireq (selectFieldList roles) "role"
         <*> ireq textField "familyName"
         <*> ireq textField "givenName"
         <*> ireq textField "email"
         <*> fmap (fmap readText) (iopt textField "avatar") --always Nothing
         <*> ireq boolField "active"
  uid <- runDB $ insert new {userPassword = Nothing}
  redirect $ UserR uid
  where
    roles = [(T.pack $ show r, r) | r <- [minBound::Role .. maxBound]]

getDeleteUserR :: UserId -> Handler RepHtml
getDeleteUserR uid = do
  (Entity _ self) <- requireAuth
  unless (isAdmin self) $
    permissionDenied "あなたはこのページを参照できません."
  user <- runDB $ get404 uid
  defaultLayout $(whamletFile "templates/deleteUser.hamlet")

postDeleteUserR :: UserId -> Handler ()
postDeleteUserR uid = do
  (Entity _ self) <- requireAuth
  unless (isAdmin self) $
    permissionDenied "あなたはこのページを参照できません."
  runDB $ delete uid
  redirect UsersR
