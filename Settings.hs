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
    , Extra (..)
    , parseExtra
    , s3dir
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
      -- for Owl service
    , clientId
    , bisocie_pub
    , bisocie_priv
    , owl_pub
    , owl_auth_service_url
    ) where

import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql (PostgresConf)
import Yesod.Default.Config
import Yesod.Default.Util
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SB
import Data.Yaml
import Control.Applicative
import Settings.Development
import Data.Default (def)
import Text.Hamlet
import Network.Mail.Mime (Address(..))
import Crypto.PubKey.RSA

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

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

-- The rest of this file contains settings which rarely need changing by a
-- user.
widgetFile :: FilePath -> Q Exp
widgetFile = (if development
              then widgetFileReload
              else widgetFileNoReload)
             widgetFileSettings
             
data Extra = Extra
    { extraCopyright :: Text
    , extraAnalytics :: Maybe Text -- ^ Google Analytics
    } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .:  "copyright"
    <*> o .:? "analytics"

s3dir :: FilePath
s3dir = "s3"

-- BISocie server settings
entryStartYear :: Integer
entryStartYear = 2000
graduateStartYear :: Integer
graduateStartYear = 2000
mailXHeader :: ByteString
mailXHeader = "X-BISocie"
mailMessageIdDomain :: Text
mailMessageIdDomain = "bisocie.localhost"
fromEmailAddress :: Address
fromEmailAddress = Address (Just "BISocie") "cutsea110@gmail.com"

projectListLimit :: Int
projectListLimit = 3
issueListLimit :: Int
issueListLimit = 3
fillGapWidth :: Int
fillGapWidth = 3
pagenateWidth :: Int
pagenateWidth = 3

-- | TimeZone. this value used as `hoursToTimeZone tz'.
tz :: Int
tz = 9

-- |
-- Owl service URL
--
owl_auth_service_url :: String
owl_auth_service_url = "http://localhost:3002/srv/auth"

-- |
-- BISocie RSA keys
--
clientId :: SB.ByteString
clientId = "BISocie"

bisocie_pub :: PublicKey
bisocie_pub = PublicKey
                { public_size = 128
                , public_n = 142887632286261757537094637659623324734697953632479544023914951183445364758392871651394986748021326605711095552963080510340887443639041675225698836993818697214651802692561889331648696803649628007583123319923152270864869553112419238211869186697095157615128026430308563152416661072674744763116497602227470107301
                , public_e = 65537
                }

bisocie_priv :: PrivateKey
bisocie_priv = PrivateKey
                { private_pub =
                     PublicKey { public_size = 128
                               , public_n = 142887632286261757537094637659623324734697953632479544023914951183445364758392871651394986748021326605711095552963080510340887443639041675225698836993818697214651802692561889331648696803649628007583123319923152270864869553112419238211869186697095157615128026430308563152416661072674744763116497602227470107301
                               , public_e = 65537
                               }
                , private_d = 122140287903148907014138253144479580618009888466746060023555235672182015947173765396383700087140130579345122357943524013455099493112329732636813930256659839379477724590925784684673874264488845395021732414679155609164122003404238428935271981186208615365053446393991027304171915484091247990330516279693573648989
                , private_p = 0
                , private_q = 0
                , private_dP = 0
                , private_dQ = 0
                , private_qinv = 0
                }
-- |
-- Owl RSA keys
--
owl_pub :: PublicKey
owl_pub = PublicKey { public_size = 128
                    , public_n = 133978865898371049756915690541771190310631304559640804303990893481160872232160722925370093358396509346866547508177130752551249861802825991982314077620630462699557927940806588373415331051489847062718976316744747135498419296507215040001805779727816051742538971179969585665983463554641712741262022247106195741053
                    , public_e = 65537
                    }
