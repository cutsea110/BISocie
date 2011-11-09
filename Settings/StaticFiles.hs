{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module Settings.StaticFiles where

import Yesod.Static

$(staticFiles "static")
