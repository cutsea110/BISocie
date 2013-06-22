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
    , PersistConf
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
    , owl_pass_service_url
    ) where

import Prelude
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
type PersistConf = PostgresConf

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
owl_pass_service_url :: String
owl_pass_service_url = "http://localhost:3002/srv/change-pass"

-- |
-- BISocie RSA keys
--
clientId :: SB.ByteString
clientId = "BISocie"

bisocie_pub :: PublicKey
bisocie_pub = PublicKey { public_size = 256
                        , public_n = 26870894169656268922545466741244168653544649756656829956589570478322104073464464636099601680532399473063611528659467583653152563017053866283270489292347012891333147755789584030406827683655185500190631673233269615494706763605175355492877596147900145933645660705964615619338455015241164432176334486419428384384974050406087406010204416327844796922320664721237134256567167659312781788762669925188944476890454877966341502697521958464868440718211676830032809781819146851835132213216793606593877127871190638938341522988710426877815716099448539090381927083488689515969622028201590093791836441861188546136053472609977446229559
                        , public_e = 65537
                        }

bisocie_priv :: PrivateKey
bisocie_priv = PrivateKey { private_pub =
                               PublicKey { public_size = 256
                                         , public_n = 26870894169656268922545466741244168653544649756656829956589570478322104073464464636099601680532399473063611528659467583653152563017053866283270489292347012891333147755789584030406827683655185500190631673233269615494706763605175355492877596147900145933645660705964615619338455015241164432176334486419428384384974050406087406010204416327844796922320664721237134256567167659312781788762669925188944476890454877966341502697521958464868440718211676830032809781819146851835132213216793606593877127871190638938341522988710426877815716099448539090381927083488689515969622028201590093791836441861188546136053472609977446229559
                                         , public_e = 65537
                                         }
                          , private_d = 22956928688423701927800223206875539726881280567083823887260732725814927890769318696917051401417360863272726446147554045444919132030576056686552603353339359427478131070912837014915371299197972005755125471352121687614082524391067234224967720309398920777129776898357642143253115564617972103117096361278517702520419364520530978951022641772314850882983369038540833237330953103009850684141168863548393902889117693566107481391481475940676819015177349782977727520522286084767169059476789554354486063616994099237895526354603027015533450348222061850116137135372795947991118924475658752444393832171207213875041959331478136355633
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
owl_pub = PublicKey { public_size = 256
                    , public_n = 23935502722801508291122222398018117881284958223263854065673689606867055652122077115632498984650750679970467900697728966520426415008444072251453446123881488809248692462117519335720631061157343736650249371835293662619945999329307142886808914215692490190245599500864907497806854772652186075160282343362861100625964817657470875052275949634580109631117392627776939182328215081842240646543745078419135398375800047086393491931547537516953037019818981085723402984601825491050312705896863144307436654552505557222743591857763940190952404403348742192979262305085887506928609325609473826220183742944601830381993567783603917096371
                    , public_e = 65537
                    }
