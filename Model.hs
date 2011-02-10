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
import Data.Int
import Data.Time
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

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
    familyName String Update
    givenName String Update
    email String Update
    active Bool Eq default=true
    UniqueUser ident
    
Profile
    user UserId
    birth Day Update
    
    entryYear Int Update
    graduateYear Int Maybe Update
    branch String Update
    
    address String Update
    longitude Double Maybe Ne Update
    latitude Double Maybe Ne Update
    tel String Update
    station String Update
    
    homeAddress String Update
    homeLongitude Double Maybe Ne Update
    homeLatitude Double Maybe Ne Update
    homeTel String Update
    
    desiredCourse String Maybe Update
    desiredWorkLocation String Maybe Update
    employment String Maybe Update
    
    UniqueProfile user
    

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
    limitdate Day Maybe Update
    cuser UserId
    cdate UTCTime
    uuser UserId Update
    udate UTCTime Update
    UniqueIssue project number

Comment
    project ProjectId Eq In
    issue IssueId Eq
    content String
    assign UserId Maybe
    status String Eq In
    limitdate Day Maybe
    cuser UserId
    cdate UTCTime Desc

Participants
    project ProjectId Eq
    user UserId Eq
    receivemail Bool Eq default=true
    UniqueParticipants project user

FileHeader
    fullname String Eq
    efname String
    name String Eq
    extension String Eq
    contentType String
    fileSize Int64
    creator UserId Eq
    created UTCTime Desc
|]

initUser :: String -> User
initUser id = User { userIdent=id
                   , userPassword=Nothing
                   , userRole=Student
                   , userActive=True
                   , userFamilyName=""
                   , userGivenName=""
                   , userEmail=""
                   }
              
userFullName :: User -> String
userFullName u = userFamilyName u ++ " " ++ userGivenName u
    
userRoleName :: User -> String
userRoleName = prettyRoleName . userRole

isStudent :: User -> Bool
isStudent u = userRole u == Student

isTeacher :: User -> Bool
isTeacher u = userRole u == Teacher

isAdmin :: User -> Bool
isAdmin u = userRole u == Admin

showLimitdate :: Issue -> String
showLimitdate i = fromMaybe "" (fmap show (issueLimitdate i))

showMaybeDouble :: Maybe Double -> String
showMaybeDouble md = case md of
  Nothing -> ""
  Just d -> show d

showDate :: UTCTime -> String
showDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

showBirthDay :: Profile -> String
showBirthDay = show . profileBirth

showEntryYear :: Profile -> String
showEntryYear = show . profileEntryYear

showGraduateYear :: Profile -> String
showGraduateYear u =  case profileGraduateYear u of
  Nothing -> ""
  Just y -> show y

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
