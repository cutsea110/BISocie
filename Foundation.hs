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
    , Route (..)
    , BISocieMessage (..)
    , resourcesBISocie
    , Handler
    , Widget
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
    , module Yesod.Goodies.PNotify
    , RawJS(..)
    , Form
    ) where

import Data.Maybe (isJust)
import qualified Database.Persist.Store
import Database.Persist.GenericSql
import Model
import Network.HTTP.Conduit (Manager)
import Network.Wai (Request(..))
import Network.Socket (getNameInfo)
import qualified Settings
import Settings (widgetFile, Extra (..))
import Text.Jasmine (minifym)
import Text.Julius (RawJS(..))
import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFile)
import Text.Julius (juliusFile)
import Web.ClientSession (getKey)
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.Owl
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form.Jquery
import Yesod.Goodies.PNotify

import Settings.StaticFiles
import BISocie.Helpers.Util

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data BISocie = BISocie
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
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
    approot = ApprootMaster $ appRoot . settings
    
    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        let timeout = 120 * 60 -- 120 minutes
        (getCachedDate, _closeDateCache) <- clientSessionDateCacher timeout
        return . Just $ clientSessionBackend2 key getCachedDate
    
    defaultLayout widget = do
      mu <- maybeAuth
      y <- getYesod
      let (ApprootMaster approot') = approot
      (title, parents) <- breadcrumbs
      current <- getCurrentRoute
      tm <- getRouteToMaster
      let header = $(hamletFile "templates/header.hamlet")
          footer = $(hamletFile "templates/footer.hamlet")
      pc <- widgetToPageContent $ do
        widget
        pnotify y
        addScriptEither $ urlJqueryJs y
        addScriptEither $ urlJqueryUiJs y
        addStylesheetEither $ urlJqueryUiCss y
        addScriptEither $ Left $ StaticR plugins_upload_jquery_upload_1_0_2_min_js
        addScriptEither $ Left $ StaticR plugins_bubbleup_jquery_bubbleup_js
        addScriptEither $ Left $ StaticR plugins_exinplaceeditor_jquery_exinplaceeditor_0_1_3_min_js
        addStylesheetEither $ Left $ StaticR plugins_exinplaceeditor_exinplaceeditor_css
        addScriptEither $ Left $ StaticR plugins_watermark_jquery_watermark_min_js
        addScriptEither $ Left $ StaticR plugins_clockpick_jquery_clockpick_1_2_9_min_js
        addStylesheetEither $ Left $ StaticR plugins_clockpick_jquery_clockpick_1_2_9_css
        addScriptEither $ Left $ StaticR plugins_ajaxzip2_ajaxzip2_js
        addScriptEither $ Left $ StaticR plugins_selection_jquery_selection_min_js
        addScriptEither $ Left $ StaticR plugins_textchange_jquery_textchange_min_js
        addScriptEither $ Left $ StaticR plugins_zClip_jquery_zclip_min_js
        addScriptEither $ Left $ StaticR plugins_pnotify_jquery_pnotify_min_js
        addStylesheetEither $ Left $ StaticR plugins_pnotify_jquery_pnotify_default_css
        toWidget $(luciusFile "templates/default-layout.lucius")
        toWidget $(juliusFile "templates/default-layout.julius")
      hamletToRepHtml $(hamletFile "templates/default-layout.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized (HomeR uid) _ = isMyOwn uid
    isAuthorized ChangePasswordR _ = loggedInAuth
    isAuthorized HumanNetworkR _ = checkUser canViewHumannetwork
    isAuthorized UserLocationsR _ = checkUser canViewUserLocations
    isAuthorized SystemBatchR _ = checkUser isAdmin
    isAuthorized (SendReminderMailR _ _ _) _ = reqFromLocalhost
    isAuthorized NewProjectR _ = checkUser canCreateProject
    isAuthorized (ProjectR pid) _ = isParticipant' pid
    isAuthorized CurrentScheduleR _ = loggedInAuth
    isAuthorized (ScheduleR _ _) _ = loggedInAuth
    isAuthorized (TaskR _ _ _) _ = loggedInAuth
    isAuthorized ProjectListR _ = loggedInAuth
    isAuthorized AssignListR _ = loggedInAuth
    isAuthorized StatusListR _ = loggedInAuth
    isAuthorized CrossSearchR _ = loggedInAuth
    isAuthorized (IssueListR pid) _ = isParticipant' pid
    isAuthorized (NewIssueR pid) _ = isParticipant pid
    isAuthorized (IssueR pid _) _ = isParticipant' pid
    isAuthorized (CommentR pid _) _ = isParticipant pid
    isAuthorized (AttachedFileR cid _) _ = canReadComment cid
    isAuthorized (CommentReadersR cid) _ = canReadComment cid
    isAuthorized (ParticipantsListR pid) _ = isParticipant' pid
    isAuthorized (ParticipantsR pid) _ = isParticipant pid
    isAuthorized _ _ = loggedInAuth

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

-- Utility functions for isAuthorized
loggedInAuth :: GHandler s BISocie AuthResult
loggedInAuth = fmap (maybe AuthenticationRequired $ const Authorized) maybeAuthId
isMyOwn :: UserId -> GHandler s BISocie AuthResult
isMyOwn uid = do
  self <- requireAuthId
  if self == uid
    then return Authorized
    else do
    r <- getMessageRender
    return $ Unauthorized $ r MsgYouCannotAccessThisPage
checkUser :: (User -> Bool) -> GHandler s BISocie AuthResult
checkUser pred = do
  u <- requireAuth
  if pred $ entityVal u
    then return Authorized
    else do
    r <- getMessageRender
    return $ Unauthorized $ r MsgYouCannotAccessThisPage
reqFromLocalhost :: GHandler s BISocie AuthResult
reqFromLocalhost = do
  req <- fmap reqWaiRequest getRequest
  (Just rhostname, _) <- liftIO $ getNameInfo [] True True $ remoteHost req
  if rhostname == "localhost"
    then return Authorized
    else do
    r <- getMessageRender
    return $ Unauthorized $ r MsgYouCannotAccessThisPage

isParticipant :: ProjectId -> GHandler s BISocie AuthResult
isParticipant pid = do
  u <- requireAuth
  mp <- runDB $ getBy $ UniqueParticipants pid (entityKey u)
  if isJust mp
    then return Authorized
    else do
    r <- getMessageRender
    return $ Unauthorized $ r MsgYouCannotAccessThisPage

isParticipant' :: ProjectId -> GHandler s BISocie AuthResult
isParticipant' pid = do
  u <- requireAuth
  mp <- runDB $ getBy $ UniqueParticipants pid (entityKey u)
  if isJust mp || isAdmin (entityVal u)
    then return Authorized
    else do
    r <- getMessageRender
    return $ Unauthorized $ r MsgYouCannotAccessThisPage

canReadComment :: CommentId -> GHandler s BISocie AuthResult
canReadComment cid = do
  u <- requireAuth
  b <- runDB $ do
    c <- get404 cid
    mp <- getBy $ UniqueParticipants (commentProject c) (entityKey u)
    return $ isJust mp || isAdmin (entityVal u)
  if b
    then return Authorized
    else do
    r <- getMessageRender
    return $ Unauthorized $ r MsgYouCannotAccessThisPage

instance YesodBreadcrumbs BISocie where
  breadcrumb RootR = return ("", Nothing)
  breadcrumb HomeR{} = return ("ホーム", Nothing)
  breadcrumb HumanNetworkR = do
    (Entity uid _) <- requireAuth
    return ("ヒューマンネットワーク", Just $ HomeR uid)
  breadcrumb (ScheduleR y m) = do
    (Entity uid _) <- requireAuth
    return (showText y +++ "年" +++ showText m +++ "月のスケジュール", Just $ HomeR uid)
  breadcrumb NewProjectR = do
    (Entity uid _) <- requireAuth
    return ("新規プロジェクト作成", Just $ HomeR uid)
  breadcrumb (ProjectR pid) = return ("設定", Just $ IssueListR pid)
    
  breadcrumb ParticipantsListR{} = return ("", Nothing)
  breadcrumb ParticipantsR{} = return ("", Nothing)
  breadcrumb UserListR = return ("ユーザ一覧", Nothing)
  
  breadcrumb CrossSearchR = do
    (Entity uid _) <- requireAuth
    return ("クロスサーチ", Just $ HomeR uid)
  breadcrumb (IssueListR pid) = do 
    (Entity uid _) <- requireAuth
    p <- runDB $ get404 pid
    return (projectName p, Just $ HomeR uid)
  breadcrumb (NewIssueR pid) = return ("案件追加", Just $ IssueListR pid)
  breadcrumb (IssueR pid ino) = do
    (Entity _ issue) <- runDB $ getBy404 $ UniqueIssue pid ino
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
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodJquery BISocie where
  urlJqueryJs _ = Left $ StaticR js_jquery_1_4_4_min_js
  urlJqueryUiJs _ = Left $ StaticR js_jquery_ui_1_8_9_custom_min_js
  urlJqueryUiCss _ = Left $ StaticR css_jquery_ui_1_8_9_custom_css

instance YesodJqueryPnotify BISocie where

instance RenderMessage BISocie FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth BISocie where
    type AuthId BISocie = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = do
      render <- getMessageRender
      runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid u) ->
              if userActive u
              then do
                lift $ setPNotify $ PNotify JqueryUI Success "Login" $ render MsgSuccessLogin
                return $ Just uid
              else do
                lift $ setPNotify $ PNotify JqueryUI Error "fail to Login" "Invalid login."
                return Nothing
            Nothing -> do
              lift $ setPNotify $ PNotify JqueryUI Success "Login" $ render MsgSuccessLogin
              fmap Just $ insert $ initUser $ credsIdent creds

    authPlugins _ = [ authOwl
                    , authGoogleEmail
                    ]
    
    authHttpManager = httpManager

    loginHandler = defaultLayout $ do
      setTitle "ログイン"
      $(widgetFile "login")


instance YesodAuthOwl BISocie where
  getOwlIdent = return . userIdent . entityVal =<< requireAuth
  clientId _ = Settings.clientId
  owlPubkey _ = Settings.owl_pub
  myPrivkey _ = Settings.bisocie_priv
  endpoint_auth _ = Settings.owl_auth_service_url
  endpoint_pass _ = Settings.owl_pass_service_url
