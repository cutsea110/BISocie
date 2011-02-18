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
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec as P (string)

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
    role Role Update Eq
    familyName String Update
    givenName String Update
    email String Update
    avatar FileHeaderId Maybe Update
    active Bool Eq default=true
    UniqueUser ident
    
Profile
    user UserId In
    birth Day Update
    
    entryYear Int Update
    graduateYear Int Maybe Update
    branch String Update
    
    zip String Update
    address String Update
    longitude Double Maybe Ne Update
    latitude Double Maybe Ne Update
    tel String Update
    station String Update
    
    homeZip String Update
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
    project ProjectId Eq In
    number IssueNo Eq Desc Asc
    subject String
    assign UserId Maybe Update In
    status String Update In
    limitdate Day Maybe Update Ge Lt
    cuser UserId
    cdate UTCTime
    uuser UserId Update
    udate UTCTime Update Desc Ge Lt
    UniqueIssue project number

Comment
    project ProjectId Eq In
    issue IssueId Eq
    content String
    assign UserId Maybe
    status String Eq In
    limitdate Day Maybe
    attached FileHeaderId Maybe
    cuser UserId
    cdate UTCTime Desc

Participants
    project ProjectId Eq In
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

data Effect = Impact | Strike deriving (Show, Eq)
type Color = String
data ProjectBis = ProjectBis { projectBisId :: ProjectId
                             , projectBisName :: String
                             , projectBisDescription :: Maybe String
                             , projectBisStatuses :: [(String, Maybe Color, Maybe Effect)]
                             }

lookupStatus :: (Eq a) => a -> [(a, b, c)] -> Maybe (a, b, c)
lookupStatus _ [] = Nothing
lookupStatus x (z@(y,_,_):zs) = if x == y
                                then Just z
                                else lookupStatus x zs

lookupProjectBis :: ProjectId -> [ProjectBis] -> Maybe ProjectBis
lookupProjectBis pid [] = Nothing
lookupProjectBis pid (p:ps) = if pid == (projectBisId p)
                              then Just p
                              else lookupProjectBis pid ps

-- | Parse Statuses
statuses :: String -> Either ParseError [(String, Maybe Color, Maybe Effect)]
statuses = mapM (parse status "status parser") . lines
status = do
  e <- effect
  s <- many1 (noneOf "#")
  c <- color
  return (s, c, e)

effect = do
  try (oneOf "!=") >>= \e ->
      case e of
        '!' -> return $ Just Impact
        '=' -> return $ Just Strike
  <|> return Nothing

color = do
  try (char '#')
  c <- color'
  return c
  <|>
  return Nothing
  where
    color' = try known
             <|>
             try rgb3
             <|>
             try rgb6
             <|> 
             do
             c <- many letter
             if c == ""
               then return Nothing
               else return $ Just c
    known = (char '赤' >> (return $ Just "#ffcccc"))
            <|>
            (char '緑' >> (return $ Just "#ccffcc"))
            <|>
            (char '青' >> (return $ Just "#ccccff"))
            <|>
            (char '灰' >> (return $ Just "#888888"))
            <|>
            (char '黄' >> (return $ Just "#ffffcc"))
      
    rgb6 = do
      r1 <- hexDigit
      r2 <- hexDigit
      g1 <- hexDigit
      g2 <- hexDigit
      b1 <- hexDigit
      b2 <- hexDigit
      eof
      return $ Just ['#',r1,r2,g1,g2,b1,b2]
    rgb3 = do
      r <- hexDigit
      g <- hexDigit
      b <- hexDigit
      eof
      return $ Just ['#',r,r,g,g,b,b]


data IssueBis = IssueBis { issueBisId :: IssueId
                         , issueBisIssue :: Issue
                         , issueBisCreator :: User
                         , issueBisUpdator :: User
                         , issueBisAssign :: Maybe User
                         }
data CommentBis = CommentBis { commentBisId :: CommentId
                             , commentBisContent :: String
                             , commentBisStatus :: String
                             , commentBisAttached :: Maybe (FileHeaderId, FileHeader)
                             , commentBisCuser :: (UserId, User)
                             , commentBisCdate :: UTCTime
                             }

initUser :: String -> User
initUser uid = User { userIdent=uid
                    , userPassword=Nothing
                    , userRole=Student
                    , userFamilyName=""
                    , userGivenName=""
                    , userEmail=""
                    , userAvatar=Nothing
                    , userActive=True
                    }

toInFilter :: ([a] -> Filter b) -> [a] -> [Filter b]
toInFilter _ [] = []
toInFilter f xs = [f xs]

maybeToFilter :: (a -> Filter b) -> Maybe a -> [Filter b]
maybeToFilter _ Nothing  = []
maybeToFilter f (Just x) = [f x]

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

toMessageId :: IssueId -> CommentId -> UTCTime -> String -> String
toMessageId (IssueId iid) (CommentId cid) time domain = "<" 
                    ++ formatTime defaultTimeLocale "%Y%m%d%H%M%S%q" time
                    ++ "i" ++ show iid 
                    ++ "c" ++ show cid 
                    ++ "@" ++ domain
                    ++ ">"

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
