{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
module BISocie
    ( BISocie (..)
    , BISocieRoute (..)
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
    , UserCrud
    , userCrud
    ) where

import Yesod
import Yesod.Helpers.Static
import Yesod.Helpers.Auth
import BISocie.Helpers.Auth.HashDB
import Yesod.Helpers.Auth.OpenId
import Yesod.Helpers.Crud
import Yesod.Form.Jquery
import System.Directory
import qualified Data.ByteString.Lazy as L
import Web.Routes.Site (Site (formatPathSegments))
import Database.Persist.GenericSql
import Settings (hamletFile, cassiusFile, juliusFile, widgetFile)
import Model
import Data.Maybe (isJust, fromMaybe)
import Control.Monad (join, unless)
import Control.Applicative ((<$>),(<*>),pure)
import Control.Arrow ((&&&))
import Network.Mail.Mime
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import Text.Jasmine (minifym)

import StaticFiles
import Model
import qualified Settings

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data BISocie = BISocie
    { getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Settings.ConnectionPool -- ^ Database connection pool.
    }

-- | A useful synonym; most of the handler functions in your application
-- will need to be of this type.
type Handler = GHandler BISocie BISocie

-- | A useful synonym; most of the widgets functions in your application
-- will need to be of this type.
type Widget = GWidget BISocie BISocie

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
--   resources declared below. This is used in Controller.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- BISocie. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the BISocieRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "BISocie" [$parseRoutes|
/ RootR GET
/home/#UserId HomeR GET
/human-network HumanNetworkR GET
/user-address.json UserLocationsR GET
/project NewProjectR GET POST
/project/#ProjectId ProjectR GET POST PUT DELETE

/participantslist/#ProjectId ParticipantsListR GET
/participants/#ProjectId ParticipantsR POST
/userlist.json UserListR GET

/cross-search CrossSearchR GET POST
/status-list StatusListR GET
/assign-list AssignListR GET

/issuelist/#ProjectId IssueListR GET
/issue/#ProjectId NewIssueR GET POST
/issue/#ProjectId/#IssueNo IssueR GET
/comment/#ProjectId/#IssueNo CommentR POST
/attached/#CommentId/#FileHeaderId AttachedFileR GET

/profile/#UserId ProfileR GET POST PUT
/avatar/#UserId AvatarR POST
/avatar-image/#UserId AvatarImageR GET

/static StaticR Static getStatic
/auth   AuthR   Auth   getAuth

/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/admin AdminR UserCrud userCrud

/s3/upload UploadR POST PUT
/s3/user/#UserId/file/#FileHeaderId FileR POST DELETE
|]
-- S3はアクセス制限する
-- S3は基本公開ベースなので制限をするURIを提供してそこからgetFileRを呼ぶ

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod BISocie where
    approot _ = Settings.approot
    
    defaultLayout widget = do
      mu <- maybeAuth
      mmsg <- getMessage
      y <- getYesod
      (title, parents) <- breadcrumbs
      current <- getCurrentRoute
      tm <- getRouteToMaster
      let header = $(Settings.hamletFile "header")
          footer = $(Settings.hamletFile "footer")
          aprt = Settings.approot
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
        addScriptEither $ Left $ StaticR plugins_ajaxzip2_ajaxzip2_js
        addScriptEither $ Left $ StaticR plugins_selection_jquery_selection_js
        addCassius $(Settings.cassiusFile "default-layout")
        addJulius $(Settings.juliusFile "default-layout")
      hamletToRepHtml $(Settings.hamletFile "default-layout")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride a (StaticR s) =
        Just $ uncurry (joinPath a Settings.staticroot) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : ext'
        let content' = content
            {--
                if ext' == "js"
                    then case minifym content of
                            Left _ -> content
                            Right y -> y
                    else content
--}
        let statictmp = Settings.staticdir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        let fn' = statictmp ++ fn
        exists <- liftIO $ doesFileExist fn'
        unless exists $ liftIO $ L.writeFile fn' content'
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", fn] [], [])

instance YesodBreadcrumbs BISocie where
  breadcrumb RootR = return ("", Nothing)
  breadcrumb HomeR{} = return ("ホーム", Nothing)
  breadcrumb HumanNetworkR = do
    (uid, _) <- requireAuth
    return ("ヒューマンネットワーク", Just $ HomeR uid)
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
    return (show (issueNumber issue) ++ ": " ++ issueSubject issue, Just $ IssueListR pid)
  breadcrumb CommentR{} = return ("", Nothing)
  
  breadcrumb (ProfileR uid) = do 
    u <- runDB $ get404 uid
    mode <- lookupGetParam "mode"
    case mode of
      Just "e" -> return (userFullName u ++ " プロフィール編集", Nothing)
      _        -> return (userFullName u, Nothing)
  
  -- these pages never call breadcrumb
  breadcrumb StaticR{} = return ("", Nothing)
  breadcrumb AuthR{} = return ("", Nothing)
  
  breadcrumb FaviconR = return ("", Nothing)
  breadcrumb RobotsR = return ("", Nothing)
  
  breadcrumb UserLocationsR = return ("", Nothing)
  
  breadcrumb AdminR{} = return ("", Nothing)  
  
  breadcrumb UploadR = return ("", Nothing)
  

-- How to run database actions.
instance YesodPersist BISocie where
    type YesodDB BISocie = SqlPersist
    runDB db = liftIOHandler 
               $ fmap connPool getYesod >>= Settings.runConnectionPool db

instance YesodJquery BISocie where
  urlJqueryJs _ = Left $ StaticR js_jquery_1_4_4_min_js
  urlJqueryUiJs _ = Left $ StaticR js_jquery_ui_1_8_9_custom_min_js
  urlJqueryUiCss _ = Left $ StaticR css_jquery_ui_1_8_9_custom_css

instance Item User where
  itemTitle = userIdent

type UserCrud = Crud BISocie User

instance ToForm User BISocie where
  toForm mu = fieldsToTable $ User
              <$> stringField "ident" (fmap userIdent mu)
              <*> maybePasswordField "password" Nothing
              <*> selectField roleopts "role" (fmap userRole mu)
              <*> stringField "familyName" (fmap userFamilyName mu)
              <*> stringField "givenName" (fmap userGivenName mu)
              <*> emailField "email" (fmap userEmail mu)
              <*> pure (fromMaybe Nothing (fmap userAvatar mu))
              <*> boolField "active" (fmap userActive mu)
    where
      roleopts = map (id &&& show) [minBound..maxBound]

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
                runDB $ delete k
           }


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

    showAuthId _ = showIntegral
    readAuthId _ = readIntegral

    authPlugins = [ authHashDB, authOpenId ]
    
    loginHandler = do
      defaultLayout $ do
        addCassius $(Settings.cassiusFile "login")
        addHamlet $(Settings.hamletFile "login")
                  
instance YesodAuthHashDB BISocie where
    type AuthHashDBId BISocie = UserId

    showAuthHashDBId _ = showIntegral
    readAuthHashDBId _ = readIntegral

    getPassword uid = runDB $ do
      ma <- get uid
      case ma of
        Nothing -> return Nothing
        Just u -> return $ userPassword u
    setPassword uid encripted = runDB $ update uid [UserPassword $ Just encripted]
    getHashDBCreds account = runDB $ do
        ma <- getBy $ UniqueUser account
        case ma of
            Nothing -> return Nothing
            Just (uid, _) -> return $ Just HashDBCreds
                { hashdbCredsId = uid
                , hashdbCredsAuthId = Just uid
                }
    getHashDB = runDB . fmap (fmap userIdent) . get
