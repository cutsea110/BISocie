{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Profile where

import BISocie
import Settings (hamletFile, cassiusFile, juliusFile, widgetFile)

import Control.Monad
import Control.Applicative ((<$>),(<*>))

getProfileR :: UserId -> Handler RepHtml
getProfileR uid = do
  (uid', _) <- requireAuth
  when (uid' /= uid) $ do
    permissionDenied "You couldn't access another user profile."
  u <- runDB $ get404 uid
  defaultLayout $ do
    setTitle $ string "Profile"
    addJulius $(juliusFile "profile")
    addHamlet $(hamletFile "viewProfile")

postProfileR :: UserId -> Handler ()
postProfileR uid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "modify" -> putProfileR uid
    _             ->invalidArgs ["The possible values of '_method' is modify"]

putProfileR :: UserId -> Handler ()
putProfileR uid = do
  (uid', u) <- requireAuth
  when (uid' /= uid) $ do
    permissionDenied "You couldn't access another user profile."
  fn' <- lookupPostParam "familyname"
  ln' <- lookupPostParam "givenname"
  let (fn, ln) = ( fn' `mplus` userFamilyname u
                 , ln' `mplus` userGivenname u
                 )
  runDB $ update uid [UserFamilyname fn, UserGivenname ln]
  setMessage "Your profile updated."
  redirect RedirectTemporary $ ProfileR uid
