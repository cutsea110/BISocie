{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
    ( withBISocie
    , withDevelApp
    ) where

import BISocie
import Settings
import Yesod.Static
import Yesod.Auth
import Database.Persist.GenericSql
import Data.ByteString (ByteString)
import Network.Wai
import Data.Dynamic (Dynamic, toDyn)

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
getFaviconR = sendFile "image/x-icon" "favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withBISocie :: (Application -> IO a) -> IO a
withBISocie f = Settings.withConnectionPool $ \p -> do
    runConnectionPool (runMigration migrateAll) p
    s' <- s
    http <- toWaiApp $ BISocie s' p False
    https <- toWaiApp $ BISocie s' p True
    f $ \req -> (if isSecure req then https else http) req
  where
    s = static Settings.staticdir

withDevelApp :: Dynamic
withDevelApp = toDyn (withBISocie :: (Application -> IO ()) -> IO ())
