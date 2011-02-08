{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
module Handler.Profile where

import BISocie
import Settings (hamletFile, cassiusFile, juliusFile, widgetFile)

import Control.Monad
import Control.Applicative ((<$>),(<*>))

getProfileR :: UserId -> Handler RepHtml
getProfileR uid = do
  (selfid, self) <- requireAuth
  (user, viewable, editable, cancreateproject) <- runDB $ do
    user <- get404 uid
    let viewable = self == user || userRole self > userRole user
        editable = self == user || userRole self > userRole user
        cancreateproject = userRole self >= Teacher
    unless viewable $ lift $ permissionDenied "あなたはこのユーザプロファイルを見ることはできません."
    return (user, viewable, editable, cancreateproject)
  defaultLayout $ do
    setTitle $ string "Profile"
    addHamlet $(hamletFile "profile")

postProfileR :: UserId -> Handler RepJson
postProfileR uid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "modify" -> putProfileR uid
    _             ->invalidArgs ["The possible values of '_method' is modify"]

putProfileR :: UserId -> Handler RepJson
putProfileR uid = do
  (selfid, self) <- requireAuth
  user <- runDB $ do
    -- validate
    user <- get404 uid
    let editable = selfid == uid || userRole self > userRole user
    unless editable $ lift $ permissionDenied "あなたはこのユーザプロファイルを編集することはできません."
    -- logic
    fn <- (lift $ lookupPostParam "familyname") >>= 
           \fn' -> return $ fn' `mplus` userFamilyname user
    gn <- (lift $ lookupPostParam "givenname") >>=
          \gn' -> return $ gn' `mplus` userGivenname user
    em <- (lift $ lookupPostParam "email") >>=
          \em' -> return $ em' `mplus` userEmail user
    update uid [UserFamilyname fn, UserGivenname gn, UserEmail em]
    get404 uid
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [ ("id", showJScalar uid)
                          , ("ident", jsonScalar $ userIdent user)
                          , ("familyname", showMaybeJScalar $ userFamilyname user)
                          , ("givenname", showMaybeJScalar $ userGivenname user)
                          , ("role", showJScalar $ userRole user)
                          , ("email", showMaybeJScalar $ userEmail user)
                          ]
    where
      showJScalar :: (Show a) => a -> Json
      showJScalar = jsonScalar . show
      showMaybeJScalar :: Maybe String -> Json
      showMaybeJScalar = jsonScalar . showmaybe
