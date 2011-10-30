{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module StaticFiles where

import Yesod.Static

$(staticFiles "static")
