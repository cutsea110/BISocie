{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Profile where

import BISocie
import Settings (hamletFile, cassiusFile, juliusFile, widgetFile)

import Control.Monad
import Control.Applicative ((<$>),(<*>))

getProfileR :: UserId -> Handler RepHtml
getProfileR uid = do
  (uid', u') <- requireAuth
  u <- runDB $ get404 uid
  let viewable = u' `canView` u
      editable = u' `canEdit` u
  when (not viewable) $ do
    permissionDenied "あなたはこのユーザプロファイルを見ることはできません."
  defaultLayout $ do
    setTitle $ string "Profile"
    addHamlet $(hamletFile "profile")

postProfileR :: UserId -> Handler ()
postProfileR uid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "modify" -> putProfileR uid
    _             ->invalidArgs ["The possible values of '_method' is modify"]

putProfileR :: UserId -> Handler ()
putProfileR uid = do
  (uid', u') <- requireAuth
  u <- runDB $ get404 uid
  let editable = u' `canEdit` u
  when (not editable ) $ do
    permissionDenied "You couldn't modify another user profile."
  fn' <- lookupPostParam "familyname"
  ln' <- lookupPostParam "givenname"
  em' <- lookupPostParam "email"
  let (fn, ln, em) = ( fn' `mplus` userFamilyname u
                     , ln' `mplus` userGivenname u
                     , em' `mplus` userEmail u  
                     )
  runDB $ update uid [UserFamilyname fn, UserGivenname ln, UserEmail em]
  setMessage "プロフィールを更新しました."
  redirect RedirectTemporary $ ProfileR uid
