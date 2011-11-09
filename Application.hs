{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( withBISocie
    , withDevelAppPort
    ) where

import Foundation
import Settings
import Yesod.Static
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Logger (Logger)
import Data.Dynamic (Dynamic, toDyn)
import qualified Database.Persist.Base
import Database.Persist.GenericSql (runMigration)
import Data.ByteString (ByteString)

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Profile
import Handler.Project
import Handler.Issue
import Handler.Participants
import Handler.User
import Handler.S3

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in BISocie.hs. Please see
-- the comments there for more details.
mkYesodDispatch "BISocie" resourcesBISocie

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withBISocie :: AppConfig DefaultEnv -> Logger -> (Application -> IO ()) -> IO ()
withBISocie conf logger f = do
#ifdef PRODUCTION
  s <- static Settings.staticdir
#else
  s <- staticDevel Settings.staticdir
#endif
  dbconf <- withYamlEnvironment "config/postgres.yml" (appEnv conf)
            $ either error return . Database.Persist.Base.loadConfig
  Database.Persist.Base.withPool (dbconf :: Settings.PersistConfig) $ \p -> do
    Database.Persist.Base.runPool dbconf (runMigration migrateAll) p
    let h = BISocie conf logger s p
    defaultRunner f h

withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withBISocie
