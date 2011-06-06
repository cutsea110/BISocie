{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module StaticFiles where

import Yesod.Helpers.Static

$(staticFiles "static")
