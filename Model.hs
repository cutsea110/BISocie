{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Model where

import Yesod
import Database.Persist.TH (share2, derivePersistField)
import Database.Persist.Base
import Database.Persist.GenericSql (mkMigrate)
import System.Locale
import Data.Time
import Data.Int
import Data.Monoid
import Control.Monad
import Control.Failure
import Control.Monad.Trans.Class
import Data.List (intercalate)
import Data.List.Split (splitOn)

data Role =  Student | Teacher | Admin
          deriving (Read, Show, Eq, Ord, Enum, Bounded)
derivePersistField "Role"

prettyRoleName :: Role -> String
prettyRoleName Admin = "管理者"
prettyRoleName Teacher = "教職員"
prettyRoleName Student = "在校生/卒業生"

type IssueNo = Int

-- You can define all of your database entities here. You can find more
-- information on persistent and how to declare entities at:
-- http://docs.yesodweb.com/book/persistent/
share2 mkPersist (mkMigrate "migrateAll") [$persist|
User
    ident String
    password String Maybe Update
    role Role Update
    email String Maybe Update
    familyname String Maybe Update
    givenname String Maybe Update
    active Bool Eq default=true
    UniqueUser ident

Email
    email String
    user UserId Maybe Update
    verkey String Maybe Update
    UniqueEmail email

Project
    name String Update
    description String Maybe Update
    statuses String Update
    issuecounter IssueNo Update Add default=0
    cuser UserId
    cdate UTCTime
    udate UTCTime Update

Issue
    project ProjectId Eq
    number IssueNo Eq
    subject String
    assign UserId Maybe Update
    status String Update
    limitdate UTCTime Maybe Update
    cuser UserId
    cdate UTCTime
    uuser UserId Update
    udate UTCTime Update
    UniqueIssue project number

Comment
    project ProjectId Eq In
    issue IssueId Eq
    content String
    status String Eq In
    cuser UserId
    cdate UTCTime Desc

Participants
    project ProjectId Eq
    user UserId Eq
    receivemail Bool Eq default=true
    UniqueParticipants project user
|]

initUser :: User
initUser = User { userIdent=""
                , userPassword=Nothing
                , userRole=Student
                , userEmail=Nothing
                , userFamilyname=Nothing
                , userGivenname=Nothing
                , userActive=True
                }

initProject :: UserId -> UTCTime -> Project
initProject u d = Project { projectName=""
                          , projectIssuecounter=0
                          , projectDescription=Nothing
                          , projectStatuses=""
                          , projectCuser=u
                          , projectCdate=d
                          , projectUdate=d
                          }
                  
initIssue :: UserId -> ProjectId -> IssueNo -> String -> UTCTime -> Issue
initIssue uid pid ino sbj d = Issue { issueProject=pid
                                    , issueNumber=ino
                                    , issueSubject=sbj
                                    , issueAssign=Nothing
                                    , issueStatus=""
                                    , issueLimitdate=Nothing
                                    , issueCuser=uid
                                    , issueCdate=d
                                    , issueUuser=uid
                                    , issueUdate=d
                                    }

initComment :: UserId -> ProjectId -> IssueId -> String -> UTCTime -> Comment
initComment uid pid iid cntnt d = Comment { commentProject=pid
                                          , commentIssue=iid
                                          , commentContent=cntnt
                                          , commentStatus=""
                                          , commentCuser=uid
                                          , commentCdate=d
                                          }

userDisplayName :: User -> String
userDisplayName u = name
  where 
    Just name = fullname `mplus` ident
    fullname = userFamilyname u `mappend` userGivenname u
    ident = Just $ userIdent u
    
userRoleName :: User -> String
userRoleName = prettyRoleName . userRole

isStudent :: User -> Bool
isStudent u = userRole u == Student

isTeacher :: User -> Bool
isTeacher u = userRole u == Teacher

isAdmin :: User -> Bool
isAdmin u = userRole u == Admin

class Permitable o where
  canEdit :: (PersistBackend (t m), Failure ErrorResponse m, MonadTrans t) 
             => Key User -> Key o -> t m Bool
  canView :: (PersistBackend (t m), Failure ErrorResponse m, MonadTrans t) 
             => Key User -> Key o -> t m Bool
  canView = canEdit

class (Permitable o) => PermitableContainer o where
  canViewChild :: (PersistBackend (t m), Failure ErrorResponse m, MonadTrans t) 
             => Key User -> Key o -> t m Bool
  canViewChild = canView
  canAddChild :: (PersistBackend (t m), Failure ErrorResponse m, MonadTrans t) 
             => Key User -> Key o -> t m Bool
  canAddChild = canView
                 
instance Permitable User where
  uid `canEdit` uid' = do
    u <- get404 uid
    u' <- get404 uid'
    return $ u == u' || userRole u > userRole u'

instance Permitable Project where
  uid `canEdit` pid = do
    u <- get404 uid
    p <- getBy $ UniqueParticipants pid uid
    return $ userRole u >= Teacher && p /= Nothing
  uid `canView` pid = do
    p <- getBy $ UniqueParticipants pid uid
    return $ p /= Nothing
    
instance PermitableContainer Project where

canSearchUser :: User -> Bool
canSearchUser u = userRole u >= Teacher

canCreateProject :: User -> Bool
canCreateProject u = userRole u >= Teacher

showDate :: UTCTime -> String
showDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

showmaybe :: Maybe String -> String
showmaybe Nothing  = ""
showmaybe (Just x) = x

showMultilineText :: String -> Html
showMultilineText = preEscapedString . intercalate "<br/>" . splitOn "\n"

showShortenText :: String -> Html
showShortenText = preEscapedString . shorten 16 . safeHead . splitOn "\n"
  where
    safeHead [] = []
    safeHead s = head s
    shorten n s = if length s > n then take n s ++ ".." else s
