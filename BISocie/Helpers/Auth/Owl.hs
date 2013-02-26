{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module BISocie.Helpers.Auth.Owl
       ( authOwl
       , YesodAuthOwl(..)
       , ServiceURL
       , loginR
       , setPassR
       ) where

import Yesod hiding (object)
import Yesod.Auth

import Control.Applicative ((<$>),(<*>))
import qualified Data.Text as T
import Data.Conduit as C
import Network.HTTP.Conduit

import Data.Aeson
import Data.Conduit.Binary (sourceLbs)
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.ByteString.Char8 as SB
import Data.Text (Text)
import qualified Yesod.Goodies.PNotify as P
import BISocie.Helpers.Util
import Crypto.PubKey.RSA
import Owl.Service.API.Auth as A
import Owl.Service.API.ChangePass as CP

type ServiceURL = String

class YesodAuth m => YesodAuthOwl m where
  getOwlIdent :: GHandler Auth m Text
  clientId :: m -> SB.ByteString
  owlPubkey :: m -> PublicKey
  myPrivkey :: m -> PrivateKey
  endpoint_auth :: m -> ServiceURL
  endpoint_pass :: m -> ServiceURL

loginR :: AuthRoute
loginR = PluginR "owl" ["login"]

setPassR :: AuthRoute
setPassR = PluginR "owl" ["set-password"]

authOwl :: YesodAuthOwl m => AuthPlugin m
authOwl =  AuthPlugin "owl" dispatch login
  where
    dispatch "POST" ["login"] = do
      y <- getYesod
      let (clid, owlpub, mypriv, ep) = (clientId y, owlPubkey y, myPrivkey y, endpoint_auth y)
      oreq <- getRequest
      (ident, pass) <- (,) <$> (runInputPost $ ireq textField "ident")
                           <*> (runInputPost $ ireq passwordField "password")
      req' <- lift $ parseUrl ep
      (e, _) <- liftIO $ encrypt owlpub $ encode $ AuthReq ident pass
      let req = req' { requestHeaders =
                          [ ("Content-Type", "application/json")
                          , ("X-Owl-clientId", clid)
                          , ("X-Owl-signature", fromLazy $ sign mypriv e)
                          , ("Accept-Language", SB.pack $ T.unpack $ T.intercalate ";" $ reqLangs oreq)
                          ]
                     , method = "POST"
                     , requestBody = RequestBodyLBS e
                     }
      res <- http req =<< authHttpManager <$> getYesod
      v <- responseBody res $$+- sinkParser json
      case fromJSON v of
        Success (OwlRes e) -> do
          let plain = decrypt mypriv $ fromLazy e
          v' <- sourceLbs (toLazy plain) $$ sinkParser json
          case fromJSON v' of
            Success (A.Accepted i e) ->
              setCreds True $ Creds "owl" ident []
            Success (A.Rejected i p r) -> do
              P.setPNotify $ P.PNotify P.JqueryUI P.Error "login failed" r
              toMaster <- getRouteToMaster
              redirect $ toMaster LoginR
        Error msg -> invalidArgs [T.pack msg]
    dispatch "GET" ["set-password"] = getPasswordR >>= sendResponse
    dispatch "POST" ["set-password"] = postPasswordR >>= sendResponse
    dispatch _ _ = notFound
    login authToMaster =
      toWidget [hamlet|
<form method="post" action="@{authToMaster loginR}" .form-horizontal>
  <div .control-group.info>
    <label .control-label for=ident>Owl Account ID
    <div .controls>
      <input type=text #ident name=ident .span3 autofocus="" required>
  <div .control-group.info>
    <label .control-label for=ident>Owl Password
    <div .controls>
      <input type=password #password name=password .span3 required>
  <div .control-group>
    <div .controls.btn-group>
      <input type=submit .btn.btn-primary value=Login>
|]

getPasswordR :: Yesod master => GHandler Auth master RepHtml
getPasswordR = do
  authToMaster <- getRouteToMaster
  defaultLayout $ do
    setTitle "Set password"
    [whamlet|
<form method="post" action="@{authToMaster setPassR}" .form-horizontal>
  <div .control-group.info>
    <label .control-label for=current_pass>Current Password
    <div .controls>
      <input type=password #current_pass name=current_pass .span3 autofocus="" required>
  <div .control-group.info>
    <label .control-label for=new_pass>New Password
    <div .controls>
      <input type=password #new_pass name=new_pass .span3 required>
  <div .control-group.info>
    <label .control-label for=new_pass2>Confirm
    <div .controls>
      <input type=password #new_pass2 name=new_pass2 .span3 required>
  <div .control-group>
    <div .controls.btn-group>
      <input type=submit .btn.btn-primary value="Set password">
|]

postPasswordR :: YesodAuthOwl master => GHandler Auth master ()
postPasswordR = do
  uid <- getOwlIdent
  y <- getYesod
  let (clid, owlpub, mypriv, ep) = (clientId y, owlPubkey y, myPrivkey y, endpoint_pass y)
  oreq <- getRequest
  (curp, pass, pass2) <- (,,)
                        <$> (runInputPost $ ireq passwordField "current_pass")
                        <*> (runInputPost $ ireq passwordField "new_pass")
                        <*> (runInputPost $ ireq passwordField "new_pass2")
  req' <- lift $ parseUrl ep
  (e, _) <- liftIO $ encrypt owlpub $ encode $ ChangePassReq uid curp pass pass2
  let req = req' { requestHeaders =
                      [ ("Content-Type", "application/json")
                      , ("X-Owl-clientId", clid)
                      , ("X-Owl-signature", fromLazy $ sign mypriv e)
                      , ("Accept-Language", SB.pack $ T.unpack $ T.intercalate ";" $ reqLangs oreq)
                      ]
                 , method = "POST"
                 , requestBody = RequestBodyLBS e
                 }
  res <- http req =<< authHttpManager <$> getYesod
  v <- responseBody res $$+- sinkParser json
  case fromJSON v of
    Success (OwlRes e) -> do
      let plain = decrypt mypriv $ fromLazy e
      v' <- sourceLbs (toLazy plain) $$ sinkParser json
      case fromJSON v' of
        Success (CP.Accepted i e) -> do
          P.setPNotify $ P.PNotify P.JqueryUI P.Success "success" "update password"
        Success (CP.Rejected i c p p2 r) -> do
          P.setPNotify $ P.PNotify P.JqueryUI P.Error "failed" r
        Error msg -> invalidArgs [T.pack msg]
  redirect $ loginDest y
