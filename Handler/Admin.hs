{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Admin where

import Foundation
import BISocie.Helpers.Auth.HashDB (encrypt)
import BISocie.Helpers.Util

import qualified Data.Text as T
import Control.Applicative ((<$>),(<*>))
import Control.Monad (unless)

getUsersR :: Handler RepHtml
getUsersR = do
  (_, self) <- requireAuth
  unless (isAdmin self) $
    permissionDenied "あなたはこのページにアクセスできません."
  users <- runDB $ selectList [] [Asc UserIdent]
  defaultLayout $ do
    addWidget $(whamletFile "templates/users.hamlet")

getUserR :: UserId -> Handler RepHtml
getUserR uid = do
  (_, self) <- requireAuth
  unless (isAdmin self) $
    permissionDenied "あなたはこのページを参照できません."
  user <- runDB $ get404 uid
  let roleIs r = r == userRole user
      toInt = (+1) . fromEnum
  defaultLayout $(whamletFile "templates/user.hamlet")
  where
    roles = [(T.pack $ show r, r) | r <- [minBound::Role .. maxBound]]
    

postUserR :: UserId -> Handler ()
postUserR uid = do
  (_, self) <- requireAuth
  unless (isAdmin self) $
    permissionDenied "あなたはこのページを参照できません."
  new <- runInputPost $ User
         <$> ireq textField "ident"
         <*> iopt passwordField "password"
         <*> ireq (selectField roles) "role"
         <*> ireq textField "familyName"
         <*> ireq textField "givenName"
         <*> ireq textField "email"
         <*> fmap (fmap readText) (iopt textField "avatar") -- always Nothing
         <*> ireq boolField "active"
  runDB $ do 
    orig <- get404 uid
    replace uid new { userPassword = pass orig new 
                    , userAvatar = userAvatar orig
                    }
  redirect RedirectSeeOther (UserR uid)
  where
    roles = [(T.pack $ show r, r) | r <- [minBound::Role .. maxBound]]
    pass :: User -> User -> Maybe T.Text
    pass old new = maybe (userPassword old) (return . encrypt) (userPassword new)

getNewUserR :: Handler RepHtml
getNewUserR = do
  (_, self) <- requireAuth
  unless (isAdmin self) $
    permissionDenied "あなたはこのページを参照できません."
  let toInt = (+1) . fromEnum
  defaultLayout $ do
    addWidget $(whamletFile "templates/newUser.hamlet")
  where
    roles = [(T.pack $ show r, r) | r <- [minBound::Role .. maxBound]]

postNewUserR :: Handler ()
postNewUserR = do
  (_, self) <- requireAuth
  unless (isAdmin self) $
    permissionDenied "あなたはこのページを参照できません."
  new <- runInputPost $ User
         <$> ireq textField "ident"
         <*> iopt passwordField "password"
         <*> ireq (selectField roles) "role"
         <*> ireq textField "familyName"
         <*> ireq textField "givenName"
         <*> ireq textField "email"
         <*> fmap (fmap readText) (iopt textField "avatar") --always Nothing
         <*> ireq boolField "active"
  uid <- runDB $ insert new {userPassword = fmap encrypt (userPassword new)}
  redirect RedirectSeeOther (UserR uid)
  where
    roles = [(T.pack $ show r, r) | r <- [minBound::Role .. maxBound]]

getDeleteUserR :: UserId -> Handler RepHtml
getDeleteUserR uid = do
  (_, self) <- requireAuth
  unless (isAdmin self) $
    permissionDenied "あなたはこのページを参照できません."
  user <- runDB $ get404 uid
  defaultLayout $ do
    addWidget $(whamletFile "templates/deleteUser.hamlet")

postDeleteUserR :: UserId -> Handler ()
postDeleteUserR uid = do
  (_, self) <- requireAuth
  unless (isAdmin self) $
    permissionDenied "あなたはこのページを参照できません."
  runDB $ delete uid
  redirect RedirectSeeOther UsersR
