module Handler.Admin where

import Import
import BISocie.Helpers.Util
import qualified Data.Text as T

getUsersR :: Handler Html
getUsersR = do
  users <- runDB $ selectList [] [Asc UserIdent]
  defaultLayout $(widgetFile "users")

getUserR :: UserId -> Handler Html
getUserR uid = do
  user <- runDB $ get404 uid
  let roleIs r = r == userRole user
      toInt = (+1) . fromEnum
  defaultLayout $(widgetFile "user")
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

getNewUserR :: Handler Html
getNewUserR = do
  let toInt = (+1) . fromEnum
  defaultLayout $(widgetFile "newUser")
  where
    roles = [(T.pack $ show r, r) | r <- [minBound::Role .. maxBound]]

postNewUserR :: Handler ()
postNewUserR = do
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

getDeleteUserR :: UserId -> Handler Html
getDeleteUserR uid = do
  user <- runDB $ get404 uid
  defaultLayout $(widgetFile "deleteUser")

postDeleteUserR :: UserId -> Handler ()
postDeleteUserR uid = do
  runDB $ delete uid
  redirect UsersR
