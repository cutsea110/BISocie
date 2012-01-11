{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the BISocie.hs file.
module Settings
    ( widgetFile
    , PersistConfig
    , staticRoot
    , staticDir
    , s3dir
    , s3root
      --
    , entryStartYear
    , graduateStartYear
    , mailXHeader
    , mailMessageIdDomain
    , fromEmailAddress
    , projectListLimit
    , issueListLimit
    , fillGapWidth
    , pagenateWidth
    , tz
    ) where

import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql (PostgresConf)
import Yesod.Default.Config
import qualified Yesod.Default.Util
import Data.Text (Text)
import Data.ByteString (ByteString)
import Network.Mail.Mime (Address(..))

-- | Which Persistent backend this site is using.
type PersistConfig = PostgresConf

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in BISocie.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in BISocie.hs
staticRoot :: AppConfig DefaultEnv x -> Text
staticRoot conf = [st|#{appRoot conf}/static|]

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: FilePath -> Q Exp
#if DEVELOPMENT
widgetFile = Yesod.Default.Util.widgetFileReload
#else
widgetFile = Yesod.Default.Util.widgetFileNoReload
#endif

s3dir :: FilePath
s3dir = "s3"
s3root :: AppConfig DefaultEnv -> Text
s3root conf = [st|#{appRoot conf}/s3|]

-- BISocie server settings
entryStartYear :: Integer
entryStartYear = 2000
graduateStartYear :: Integer
graduateStartYear = 2000
mailXHeader :: ByteString
mailXHeader = "X-BISocie-Soubun"
mailMessageIdDomain :: Text
mailMessageIdDomain = "bisocie.seitoku.ac.jp"
fromEmailAddress :: Address
fromEmailAddress = Address (Just "BISocie") "bisocie@seitoku.ac.jp"

projectListLimit :: Int
projectListLimit = 50
issueListLimit :: Int
issueListLimit = 50
fillGapWidth :: Int
fillGapWidth = 3
pagenateWidth :: Int
pagenateWidth = 3

-- | TimeZone. this value used as `hoursToTimeZone tz'.
tz :: Int
tz = 9
