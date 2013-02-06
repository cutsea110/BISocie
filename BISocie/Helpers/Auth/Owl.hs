{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module BISocie.Helpers.Auth.Owl 
       ( authOwl
       ) where

import Yesod
import Yesod.Auth

authOwl :: YesodAuth m => AuthPlugin m
authOwl =  AuthPlugin "owl" dispatch login
  where
    dispatch "POST" [] = do
      ident <- runInputPost $ ireq textField "ident"
      setCreds True $ Creds "owl" ident []
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
