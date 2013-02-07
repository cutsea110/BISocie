{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module BISocie.Helpers.Auth.Owl 
       ( authOwl
       ) where

import Yesod hiding (object)
import Yesod.Auth

import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Conduit as C
import Network.HTTP.Conduit as C -- (http, parseUrl, Request(..), Response(..))

import Data.Aeson
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.HashMap.Strict as M (toList)
import qualified Yesod.Goodies.PNotify as P

-- for Request
data AuthReq = AuthReq
               { ident :: Text
               , pass :: Text
               }
             deriving (Show, Read, Eq)

instance FromJSON AuthReq where
  parseJSON (Object v) = AuthReq <$> v .: "ident" <*> v .: "pass"
  parseJSON _ = mzero
instance ToJSON AuthReq where
  toJSON (AuthReq i p) = object ["ident" .= i, "pass" .= p]

-- for Response
data AuthRes = Rejected { rejected_ident :: Text, rejected_pass :: Text }
             | Accepted { accepted_ident :: Text, accepted_email :: Maybe Text }
             deriving (Show, Read, Eq)

instance FromJSON AuthRes where
  parseJSON (Object o) = case M.toList o of
    [("rejected", Object o')] -> Rejected <$> o' .: "ident" <*> o' .: "pass"
    [("accepted", Object o')] -> Accepted <$> o' .: "ident" <*> o' .:? "email"
    _ -> error $ show o
  parseJSON _ = mzero

instance ToJSON AuthRes where
  toJSON (Rejected i p) = object [ "rejected" .= object [ "ident" .= i
                                                        , "pass" .= p
                                                        ]
                                 ]
  toJSON (Accepted i me) = object [ "accepted" .= object [ "ident" .= i
                                                         , "email" .= me
                                                         ]
                                  ]

owl_auth_url = "http://localhost:3002/srv/auth"

authOwl :: YesodAuth m => AuthPlugin m
authOwl =  AuthPlugin "owl" dispatch login
  where
    dispatch "POST" [] = do
      (ident, pass) <- (,) <$> (runInputPost $ ireq textField "ident")
                           <*> (runInputPost $ ireq passwordField "password")
      req' <- lift $ parseUrl owl_auth_url
      let req = req' { requestHeaders = [("Content-Type", "application/json")]
                     , method = "POST"
                     , requestBody = RequestBodyLBS $ encode $ AuthReq ident pass
                     }
      res <- http req =<< authHttpManager <$> getYesod
      v <- responseBody res $$+- sinkParser json
      case fromJSON v of
        Success (Accepted i e) ->
          setCreds True $ Creds "owl" ident []
        Success (Rejected i p) -> do
          P.setPNotify $ P.PNotify P.JqueryUI P.Error "login failed"
            $ T.intercalate "<br>" [ "could not accept your inputs"
                                   , "account: " `T.append` i
                                   , "password: " `T.append` p
                                   ]
          toMaster <- getRouteToMaster
          redirect $ toMaster LoginR
        Error msg -> invalidArgs [T.pack msg]
    dispatch _ _ = notFound
    url = PluginR "owl" []
    login authToMaster = 
      toWidget [hamlet|
<form method="post" action="@{authToMaster url}" .form-horizontal>
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
