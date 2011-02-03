{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Model where

import Yesod
import Database.Persist.TH (share2, derivePersistField)
import Database.Persist.Base
import Database.Persist.GenericSql (mkMigrate)
import Data.Time
import Data.Int

data Role =  Student | Teacher | Admin
          deriving (Read, Show, Eq, Ord, Enum, Bounded)
derivePersistField "Role"

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
    name String
    description String Maybe Update
    statuses String Update
    cuser UserId
    cdate UTCTime
    udate UTCTime Update

Issue
    subject String
    asign UserId Maybe
    status String
    limitdate UTCTime Maybe
    cuser UserId
    cdate UTCTime
    udate UTCTime

Post
    content String
    status String
    cuser UserId
    cdate UTCTime

Participants
    project ProjectId Eq
    user UserId Eq
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
initTeacher = initUser {userRole=Teacher}
initStudent = initUser {userRole=Student}

initProject :: UserId -> UTCTime -> Project
initProject u d = Project { projectName=""
                          , projectDescription=Nothing
                          , projectStatuses=""
                          , projectCuser=u
                          , projectCdate=d
                          , projectUdate=d
                          }
                  
initIssue :: UserId -> UTCTime -> Issue
initIssue u d = Issue { issueSubject=""
                      , issueAsign=Nothing
                      , issueStatus=""
                      , issueLimitdate=Nothing
                      , issueCuser=u
                      , issueCdate=d
                      , issueUdate=d
                      }

initPost :: UserId -> UTCTime -> Post
initPost u d = Post { postContent=""
                    , postStatus=""
                    , postCuser=u
                    , postCdate=d
                    }

userDisplayName :: User -> String
userDisplayName = userIdent

isStudent :: User -> Bool
isStudent u = userRole u == Student

isTeacher :: User -> Bool
isTeacher u = userRole u == Teacher

isAdmin :: User -> Bool
isAdmin u = userRole u == Admin

canEdit :: User -> User -> Bool
x `canEdit` y = x == y || userRole x > userRole y

canView :: User -> User -> Bool
canView = canEdit
