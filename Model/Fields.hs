module Model.Fields where

import Prelude
import Yesod

data Role =  Student | Teacher | Staff | Admin
          deriving (Read, Show, Eq, Ord, Enum, Bounded)
derivePersistField "Role"
