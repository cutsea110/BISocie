{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
module Handler.Profile where

import BISocie
import Settings (hamletFile, cassiusFile, juliusFile, widgetFile)

import Control.Monad
import Control.Applicative ((<$>),(<*>))

getProfileR :: UserId -> Handler RepHtml
getProfileR uid = do
  (uid', u') <- requireAuth
  (u, viewable, editable) <- runDB $ do
    u <- get404 uid
    v <- uid' `canView` uid
    e <- uid' `canEdit` uid
    return (u, v, e)
  when (not viewable) $ do
    permissionDenied "あなたはこのユーザプロファイルを見ることはできません."
  defaultLayout $ do
    setTitle $ string "Profile"
    addHamlet $(hamletFile "profile")

postProfileR :: UserId -> Handler RepXml
postProfileR uid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "modify" -> putProfileR uid
    _             ->invalidArgs ["The possible values of '_method' is modify"]

putProfileR :: UserId -> Handler RepXml
putProfileR uid = do
  (uid', u') <- requireAuth
  fn' <- lookupPostParam "familyname"
  gn' <- lookupPostParam "givenname"
  em' <- lookupPostParam "email"
  (fn, gn, em) <- runDB $ do
    u <- get404 uid
    editable <- uid' `canEdit` uid
    when (not editable) $ lift $ permissionDenied "あなたはこのユーザプロファイルを編集することはできません."
    let (fn, gn, em) = ( fn' `mplus` userFamilyname u
                       , gn' `mplus` userGivenname u
                       , em' `mplus` userEmail u  
                       )
    update uid [UserFamilyname fn, UserGivenname gn, UserEmail em]
    return (fn, gn, em)
  fmap RepXml $ hamletToContent
#if GHC7
                  [xhamlet|
#else
                  [$xhamlet|
#endif
%profile
  $maybe fn fn
    %familyname $fn$
  $nothing
    %familyname
  $maybe gn gn
    %givenname $gn$
  $nothing
    %givenname
  $maybe em em
    %email $em$
  $nothing
    %email
|]
