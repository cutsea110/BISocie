{-# LANGUAGE QuasiQuotes, TemplateHaskell #-} 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
module Foundation
    ( BISocie (..)
    , BISocieRoute (..)
    , BISocieMessage (..)
    , resourcesBISocie
    , Handler
    , Widget
    , maybeAuth
    , requireAuth
    , module Yesod
    , module Settings
    , module Model
    , StaticRoute (..)
    , AuthRoute (..)
      --
--    , UserCrud -- FIXME Crud
--    , userCrud -- FIXME Crud
    ) where

import Prelude
import Yesod hiding (Form, AppConfig (..), withYamlEnvironment)
import Yesod.Static (Static, base64md5, StaticRoute(..))
import Yesod.Auth
import BISocie.Helpers.Auth.HashDB
import Yesod.Auth.OpenId
-- import Yesod.Helpers.Crud -- FIXME
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText)
#ifdef DEVELOPMENT
import Yesod.Logger (logLazyText)
#endif
import qualified Settings
import qualified Data.ByteString.Lazy as L
import qualified Database.Persist.Base
import Database.Persist.GenericSql
import Settings (widgetFile)
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
import Text.Cassius (cassiusFile)
import Text.Julius (juliusFile)
import Yesod.Form.Jquery
#if DEVELOPMENT
import qualified Data.Text.Lazy.Encoding
#else
import Network.Mail.Mime (sendmail)
#endif

import Settings.StaticFiles
import qualified Settings
import BISocie.Helpers.Util

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data BISocie = BISocie
    { settings :: AppConfig DefaultEnv ()
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Base.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    }

-- Set up i18n messages. See the message folder.
mkMessage "BISocie" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://docs.yesodweb.com/book/web-routes-quasi/
--
-- This function does three things:
--
-- * Creates the route datatype BISocieRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route BISocie = BISocieRoute
-- * Creates the value resourcesBISocie which contains information on the
--   resources declared below. This is used in Application.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- BISocie. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the BISocieRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "BISocie" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm BISocie BISocie (FormResult x, Widget)

-- S3はアクセス制限する
-- S3は基本公開ベースなので制限をするURIを提供してそこからgetFileRを呼ぶ

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod BISocie where
    approot = appRoot . settings
    
    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"
    
    defaultLayout widget = do
      mu <- maybeAuth
      mmsg <- getMessage
      y <- getYesod
      (title, parents) <- breadcrumbs
      current <- getCurrentRoute
      tm <- getRouteToMaster
      let header = $(hamletFile "templates/header.hamlet")
          footer = $(hamletFile "templates/footer.hamlet")
      pc <- widgetToPageContent $ do
        widget
        addScriptEither $ urlJqueryJs y
        addScriptEither $ urlJqueryUiJs y
        addStylesheetEither $ urlJqueryUiCss y
        addScriptEither $ Left $ StaticR plugins_upload_jquery_upload_1_0_2_js
        addScriptEither $ Left $ StaticR plugins_bubbleup_jquery_bubbleup_js
        addScriptEither $ Left $ StaticR plugins_exinplaceeditor_jquery_exinplaceeditor_0_1_3_js
        addStylesheetEither $ Left $ StaticR plugins_exinplaceeditor_exinplaceeditor_css
        addScriptEither $ Left $ StaticR plugins_watermark_jquery_watermark_js
        addScriptEither $ Left $ StaticR plugins_clockpick_jquery_clockpick_1_2_9_js
        addStylesheetEither $ Left $ StaticR plugins_clockpick_jquery_clockpick_1_2_9_css
        addScriptEither $ Left $ StaticR plugins_ajaxzip2_ajaxzip2_js
        addScriptEither $ Left $ StaticR plugins_selection_jquery_selection_js
        addCassius $(cassiusFile "templates/default-layout.cassius")
        addJulius $(juliusFile "templates/default-layout.julius")
      hamletToRepHtml $(hamletFile "templates/default-layout.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Maximum allowed length of the request body, in bytes.
    maximumContentLength _ (Just (AvatarR _))      =   2 * 1024 * 1024 --  2 megabytes for default
    maximumContentLength _ (Just (AvatarImageR _)) =   2 * 1024 * 1024 --  2 megabytes for default
    maximumContentLength _ _                       =  20 * 1024 * 1024 -- 20 megabytes for default
    
    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])
    
    -- Enable Javascript async loading
--    yepnopeJs _ = Just $ Right $ StaticR js_modernizr_js

instance YesodBreadcrumbs BISocie where
  breadcrumb RootR = return ("", Nothing)
  breadcrumb HomeR{} = return ("ホーム", Nothing)
  breadcrumb HumanNetworkR = do
    (uid, _) <- requireAuth
    return ("ヒューマンネットワーク", Just $ HomeR uid)
  breadcrumb (ScheduleR y m) = do
    (uid, _) <- requireAuth
    return (showText y +++ "年" +++ showText m +++ "月のスケジュール", Just $ HomeR uid)
  breadcrumb NewProjectR = do
    (uid, _) <- requireAuth
    return ("新規プロジェクト作成", Just $ HomeR uid)
  breadcrumb (ProjectR pid) = return ("設定", Just $ IssueListR pid)
    
  breadcrumb ParticipantsListR{} = return ("", Nothing)
  breadcrumb ParticipantsR{} = return ("", Nothing)
  breadcrumb UserListR = return ("ユーザ一覧", Nothing)
  
  breadcrumb CrossSearchR = do
    (uid, _) <- requireAuth
    return ("クロスサーチ", Just $ HomeR uid)
  breadcrumb (IssueListR pid) = do 
    (uid, _) <- requireAuth
    p <- runDB $ get404 pid
    return (projectName p, Just $ HomeR uid)
  breadcrumb (NewIssueR pid) = return ("案件追加", Just $ IssueListR pid)
  breadcrumb (IssueR pid ino) = do
    (_, issue) <- runDB $ getBy404 $ UniqueIssue pid ino
    return (showText (issueNumber issue) +++ ": " +++ issueSubject issue, Just $ IssueListR pid)
  breadcrumb CommentR{} = return ("", Nothing)
  
  breadcrumb (ProfileR uid) = do 
    u <- runDB $ get404 uid
    mode <- lookupGetParam "mode"
    case mode of
      Just "e" -> return (userFullName u +++ " プロフィール編集", Nothing)
      _        -> return (userFullName u, Nothing)
  
  -- the others 
  breadcrumb _ = return ("", Nothing)
  

-- How to run database actions.
instance YesodPersist BISocie where
    type YesodPersistBackend BISocie = SqlPersist
    runDB f = liftIOHandler 
              $ fmap connPool getYesod >>= Database.Persist.Base.runPool (undefined::Settings.PersistConfig) f

instance YesodJquery BISocie where
  urlJqueryJs _ = Left $ StaticR js_jquery_1_4_4_min_js
  urlJqueryUiJs _ = Left $ StaticR js_jquery_ui_1_8_9_custom_min_js
  urlJqueryUiCss _ = Left $ StaticR css_jquery_ui_1_8_9_custom_css

{--
type UserCrud = Crud BISocie User

instance ToForm User BISocie where
  toForm mu = fieldsToTable $ User
              <$> stringField "ident" (fmap userIdent mu)
              <*> maybePasswordField "password" Nothing
              <*> selectField roleopts "role" (fmap userRole mu)
              <*> stringField "familyName" (fmap userFamilyName mu)
              <*> stringField "givenName" (fmap userGivenName mu)
              <*> stringField "email" (fmap userEmail mu)
              <*> pure (fromMaybe Nothing (fmap userAvatar mu))
              <*> boolField "active" (fmap userActive mu)
    where
      roleopts = map (id &&& showText) [minBound..maxBound]

userCrud :: BISocie -> Crud BISocie User
userCrud = const Crud
           { crudSelect = do
                (_, u) <- requireAuth
                unless (isAdmin u) $
                  permissionDenied "You couldn't access user crud."
                runDB $ selectList [] [] 0 0
           , crudReplace = \k a -> do
                (_, u) <- requireAuth
                unless (isAdmin u) $
                  permissionDenied "You couldn't access user crud."
                runDB $ do
                  case userPassword a of
                    Nothing -> do
                      Just a' <- get k
                      replace k $ a {userPassword=userPassword a', userActive=userActive a}
                    Just rp -> do
                      replace k $ a {userPassword=Just $ encrypt rp, userActive=userActive a}
           , crudInsert = \a -> do
                (_, u) <- requireAuth
                unless (isAdmin u) $
                  permissionDenied "You couldn't access user crud."
                runDB $ do
                  insert $ a { userPassword=(fmap encrypt $ userPassword a)}
           , crudGet = \k -> do
                (_, u) <- requireAuth
                unless (isAdmin u) $
                  permissionDenied "You couldn't access user crud."
                runDB $ get k
           , crudDelete = \k -> do
                (_, u) <- requireAuth
                unless (isAdmin u) $
                  permissionDenied "You couldn't access user crud."
                runDB $ do
                  deleteWhere [FileHeaderCreatorEq k]
                  deleteBy $ UniqueProfile k
                  deleteBy $ UniqueLaboratory k
                  delete k
           }
--}

instance RenderMessage BISocie FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth BISocie where
    type AuthId BISocie = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (uid, u) ->
              if userActive u
              then do
                lift $ setMessage "You are now logged in."
                return $ Just uid
              else do
                lift $ setMessage "Invalid login."
                return Nothing
            Nothing -> do
              lift $ setMessage "You are now logged in."
              fmap Just $ insert $ initUser $ credsIdent creds

    authPlugins = [ authHashDB, authOpenId ]
    
    loginHandler = do
      defaultLayout $ do
        setTitle "ログイン"
        addCassius $(cassiusFile "templates/login.cassius")
        addHamlet $(hamletFile "templates/login.hamlet")
                  
instance YesodAuthHashDB BISocie where
    type AuthHashDBId BISocie = UserId

    getPassword uid = runDB $ do
      ma <- get uid
      case ma of
        Nothing -> return Nothing
        Just u -> return $ userPassword u
    setPassword uid encripted = runDB $ update uid [UserPassword =. Just encripted]
    getHashDBCreds account = runDB $ do
        ma <- getBy $ UniqueUser account
        case ma of
            Nothing -> return Nothing
            Just (uid, _) -> return $ Just HashDBCreds
                { hashdbCredsId = uid
                , hashdbCredsAuthId = Just uid
                }
    getHashDB = runDB . fmap (fmap userIdent) . get

-- Sends off your mail. Requires sendmail in production!
deliver :: BISocie -> L.ByteString -> IO ()
#ifdef DEVELOPMENT
deliver y = logLazyText (getLogger y) . Data.Text.Lazy.Encoding.decodeUtf8
#else
deliver _ = sendmail
#endif
