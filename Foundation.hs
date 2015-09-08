{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foundation where

import Import.NoFoundation as Import
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod.Auth.Message   (AuthMessage (InvalidLogin))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

import Network.Wai (Request(..))
import Network.Socket (getNameInfo)
import Text.Julius (RawJS(..))

import Yesod.Auth.Owl
import Yesod.Form.Jquery
import Yesod.Goodies.PNotify

import BISocie.Helpers.Util

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- S3はアクセス制限する
-- S3は基本公開ベースなので制限をするURIを提供してそこからgetFileRを呼ぶ

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . appSettings
    
    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120 -- timeout in minutes
        "config/client_session_key.aes"
    
    defaultLayout widget = do
      mu <- maybeAuth
      y <- getYesod
      let (ApprootMaster approot') = approot
      (title, parents) <- breadcrumbs
      _current <- getCurrentRoute
      let header = $(widgetFile "header")
          footer = $(widgetFile "footer")
      pc <- widgetToPageContent $ do
        pnotify y
        addScriptEither $ urlJqueryJs y
        addScriptEither $ urlJqueryUiJs y
        addStylesheetEither $ urlJqueryUiCss y
        addScriptEither $ Left $ StaticR plugins_upload_jquery_upload_1_0_2_min_js
        addScriptEither $ Left $ StaticR plugins_clockpick_jquery_clockpick_1_2_9_min_js
        addStylesheetEither $ Left $ StaticR plugins_clockpick_jquery_clockpick_1_2_9_css
        addScriptEither $ Left $ StaticR plugins_ajaxzip2_ajaxzip2_js
        addScriptEither $ Left $ StaticR plugins_selection_jquery_selection_min_js
        addScriptEither $ Left $ StaticR plugins_textchange_jquery_textchange_min_js
        addScriptEither $ Left $ StaticR plugins_zClip_jquery_zclip_min_js
        addScriptEither $ Left $ StaticR plugins_placeholder_jquery_placeholder_min_js
        addScriptEither $ Left $ StaticR plugins_pnotify_jquery_pnotify_min_js
        addStylesheetEither $ Left $ StaticR plugins_pnotify_jquery_pnotify_default_css
        $(widgetFile "default-layout")
      withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

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
    isAuthorized (ParticipantsR pid _) _ = isParticipant pid
    isAuthorized (ProfileR uid) True = canEditUser uid
    isAuthorized (ProfileR _) False = loggedInAuth
    isAuthorized (AvatarImageR _) _ = loggedInAuth
    isAuthorized (AvatarR uid) _ = canEditUser uid
    isAuthorized UserListR _ = loggedInAuth
    isAuthorized UsersR _ = checkUser isAdmin
    isAuthorized (UserR _) _ = checkUser isAdmin
    isAuthorized NewUserR _ = checkUser isAdmin
    isAuthorized (DeleteUserR _) _ = checkUser isAdmin
    isAuthorized _ _ = loggedInAuth

    -- Maximum allowed length of the request body, in bytes.
    maximumContentLength _ (Just (AvatarR _))      = Just (2 * 1024 * 1024) --  2 megabytes for default
    maximumContentLength _ (Just (AvatarImageR _)) = Just (2 * 1024 * 1024) --  2 megabytes for default
    maximumContentLength _ _                       = Just (20 * 1024 * 1024) -- 20 megabytes for default
    
    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- Utility functions for isAuthorized
loggedInAuth :: Handler AuthResult
loggedInAuth = fmap (maybe AuthenticationRequired $ const Authorized) maybeAuthId
isMyOwn :: UserId -> Handler AuthResult
isMyOwn uid = do
  self <- requireAuthId
  if self == uid
    then return Authorized
    else do
    r <- getMessageRender
    return $ Unauthorized $ r MsgYouCannotAccessThisPage
checkUser :: (User -> Bool) -> Handler AuthResult
checkUser p = do
  u <- requireAuth
  if p $ entityVal u
    then return Authorized
    else do
    r <- getMessageRender
    return $ Unauthorized $ r MsgYouCannotAccessThisPage
reqFromLocalhost :: Handler AuthResult
reqFromLocalhost = do
  req <- fmap reqWaiRequest getRequest
  (Just rhostname, _) <- liftIO $ getNameInfo [] True True $ remoteHost req
  if rhostname == "localhost"
    then return Authorized
    else do
    r <- getMessageRender
    return $ Unauthorized $ r MsgYouCannotAccessThisPage

isParticipant :: ProjectId -> Handler AuthResult
isParticipant pid = do
  u <- requireAuth
  mp <- runDB $ getBy $ UniqueParticipants pid (entityKey u)
  if isJust mp
    then return Authorized
    else do
    r <- getMessageRender
    return $ Unauthorized $ r MsgYouCannotAccessThisPage

isParticipant' :: ProjectId -> Handler AuthResult
isParticipant' pid = do
  u <- requireAuth
  mp <- runDB $ getBy $ UniqueParticipants pid (entityKey u)
  if isJust mp || isAdmin (entityVal u)
    then return Authorized
    else do
    r <- getMessageRender
    return $ Unauthorized $ r MsgYouCannotAccessThisPage

canReadComment :: CommentId -> Handler AuthResult
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

canEditUser :: UserId -> Handler AuthResult
canEditUser uid = do
  u' <- requireAuth
  u <- runDB $ get404 uid
  if entityVal u' `canEdit` u
    then return Authorized
    else do
    r <- getMessageRender
    return $ Unauthorized $ r MsgYouCannotEditThisData


-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    authenticate creds = do
      render <- getMessageRender
      runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid u) ->
              if userActive u
              then do
                lift $ setPNotify $ PNotify JqueryUI Success "Login" $ render MsgSuccessLogin
                return $ Authenticated uid
              else do
                lift $ setPNotify $ PNotify JqueryUI Error "fail to Login" "Invalid login."
                return $ UserError InvalidLogin
            Nothing -> do
              lift $ setPNotify $ PNotify JqueryUI Success "Login" $ render MsgSuccessLogin
              fmap Authenticated $ insert $ initUser $ credsIdent creds

    authPlugins _ = [ authOwl
                    , authGoogleEmail
                    ]
    
    authHttpManager = getHttpManager

    loginHandler = lift $ defaultLayout $ do
      setTitle "ログイン"
      $(widgetFile "login")

instance YesodAuthPersist App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

instance YesodAuthOwl App where
  getOwlIdent = lift $ fmap (userIdent . entityVal) requireAuth
  clientId _ = Import.clientId
  owlPubkey _ = Import.owl_pub
  myPrivkey _ = Import.bisocie_priv
  endpoint_auth _ = Import.owl_auth_service_url
  endpoint_pass _ = Import.owl_pass_service_url

instance YesodJquery App where
  urlJqueryJs _ = Left $ StaticR js_jquery_1_4_4_min_js
  urlJqueryUiJs _ = Left $ StaticR js_jquery_ui_1_8_9_custom_min_js
  urlJqueryUiCss _ = Left $ StaticR css_jquery_ui_1_8_9_custom_css

instance YesodJqueryPnotify App where

instance YesodBreadcrumbs App where
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
  breadcrumb (ProjectR pid) = do
    uid <- requireAuthId
    p <- runDB $ get404 pid
    return (projectName p, Just $ HomeR uid)
    
  breadcrumb (ParticipantsListR pid) = return ("参加者", Just $ ProjectR pid)
  breadcrumb ParticipantsR{} = return ("", Nothing)
  breadcrumb UserListR = return ("ユーザ一覧", Nothing)
  
  breadcrumb CrossSearchR = do
    (Entity uid _) <- requireAuth
    return ("クロスサーチ", Just $ HomeR uid)
  breadcrumb (IssueListR pid) = return ("タスク一覧", Just $ ProjectR pid)
  breadcrumb (NewIssueR pid) = return ("タスク追加", Just $ ProjectR pid)
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

