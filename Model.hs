{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Model where

import Yesod
-- import Yesod.Crud -- FIXME
import Database.Persist.Base
import System.Locale
import Data.Char (isHexDigit)
import Data.Int
import Data.Time
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec as P (string)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze (preEscapedText)

import qualified Settings (tz)
import BISocie.Helpers.Util

type Year = Integer
type Month = Int
type Date = Int

data Role =  Student | Teacher | Staff | Admin
          deriving (Read, Show, Eq, Ord, Enum, Bounded)
derivePersistField "Role"

prettyRoleName :: Role -> Text
prettyRoleName Admin = "管理者"
prettyRoleName Staff = "職員"
prettyRoleName Teacher = "教員"
prettyRoleName Student = "在校生/卒業生"

type IssueNo = Int

-- You can define all of your database entities here. You can find more
-- information on persistent and how to declare entities at:
-- http://docs.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFile "config/models")

-- FIXME Crud
--instance Item User where
--  itemTitle = userInfoOneline

data Effect = Impact | Strike deriving (Show, Eq)
type Color = Text
data ProjectBis = ProjectBis { projectBisId :: ProjectId
                             , projectBisName :: Text
                             , projectBisDescription :: Text
                             , projectBisStatuses :: [(Text, Maybe Color, Maybe Effect)]
                             }
toProjectBis :: (ProjectId, Project) -> ProjectBis
toProjectBis (pid, prj) = 
  let Right es = parseStatuses $ projectStatuses prj
  in         
   ProjectBis { projectBisId=pid
              , projectBisName=projectName prj
              , projectBisDescription=projectDescription prj
              , projectBisStatuses=es
              }

lookupStatus :: (Eq a) => a -> [(a, b, c)] -> Maybe (a, b, c)
lookupStatus _ [] = Nothing
lookupStatus x (z@(y,_,_):zs) = if x == y
                                then Just z
                                else lookupStatus x zs

lookupProjectBis :: ProjectId -> [ProjectBis] -> Maybe ProjectBis
lookupProjectBis _   [] = Nothing
lookupProjectBis pid (p:ps) = if pid == (projectBisId p)
                              then Just p
                              else lookupProjectBis pid ps

parseStatuses :: Text -> Either ParseError [(Text, Maybe Color, Maybe Effect)]
parseStatuses t = 
    case ps (T.unpack t) of
      Right xs -> Right $ map toText xs
      Left e -> Left e
    where
      ps :: String -> Either ParseError [(String, Maybe String, Maybe Effect)]
      ps s = parse statuses "parse statuses" 
             $ if last s == '\n' then s else s ++ "\n"
      toText :: (String, Maybe String, Maybe Effect) -> (Text, Maybe Color, Maybe Effect)
      toText (s,mc,me) = (T.pack s, fmap T.pack mc, me)

eol :: CharParser st String
eol = try (P.string "\n\r")
      <|> try (P.string "\r\n")
      <|> P.string "\n"
      <|> P.string "\r"

statuses :: CharParser st [(String, Maybe String, Maybe Effect)]
statuses = endBy status eol

status :: CharParser st (String, Maybe String, Maybe Effect)
status = do
  e <- effect
  s <- many1 (noneOf "\r\n#")
  c <- color
  return (s, c, e)

effect :: CharParser st (Maybe Effect)
effect = do
  try (oneOf "!=") >>= \e ->
      case e of
        '!' -> return $ Just Impact
        '=' -> return $ Just Strike
        _   -> error "couldn't reach here." -- FIXME
  <|> return Nothing

color :: CharParser st (Maybe String)
color = do
  try (char '#')
  color'
  <|>
  (many (noneOf "\r\n") >> return Nothing)
  where
    color' = do
      c <- many (noneOf "\r\n")
      if (length c == 3 || length c == 6) && all isHexDigit c
        then return $ Just ("#" ++ (take 6 $ concat $ repeat c))
        else do
        let known = lookup c [ ("赤", "#ffcccc")
                             , ("緑", "#ccffcc")
                             , ("青", "#ccccff")
                             , ("灰", "#888888")
                             , ("黄", "#ffffcc")]
        case known of
          Nothing -> return $ Just c
          Just k  -> return $ Just k


data IssueBis = IssueBis { issueBisId :: IssueId
                         , issueBisIssue :: Issue
                         , issueBisCreator :: User
                         , issueBisUpdator :: User
                         , issueBisAssign :: Maybe User
                         }
data CommentBis = CommentBis { commentBisId :: CommentId
                             , commentBisContent :: Maybe Text
                             , commentBisStatus :: Text
                             , commentBisAttached :: Maybe (FileHeaderId, FileHeader)
                             , commentBisCuser :: (UserId, User)
                             , commentBisCdate :: UTCTime
                             }

initUser :: Text -> User
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

userInfoOneline :: User -> Text
userInfoOneline u = 
  "[" +++ showPrettyActive u +++ "] " +++ userIdent u +++ " (" +++ userFullName u +++ " , " +++ userRoleName u +++ ")"

showPrettyActive :: User -> Text
showPrettyActive u = if userActive u then "有効" else "無効"

userFullName :: User -> Text
userFullName u = userFamilyName u +++ " " +++ userGivenName u
    
userRoleName :: User -> Text
userRoleName = prettyRoleName . userRole

isStudent :: User -> Bool
isStudent u = userRole u == Student

isTeacher :: User -> Bool
isTeacher u = userRole u == Teacher

isStaff :: User -> Bool
isStaff u = userRole u == Staff

isAdmin :: User -> Bool
isAdmin u = userRole u == Admin

showLimitdate :: Issue -> Text
showLimitdate i = fromMaybe "" (fmap showText (issueLimitdate i))

showMaybeDouble :: Maybe Double -> Text
showMaybeDouble md = case md of
  Nothing -> ""
  Just d -> showText d

showDate :: UTCTime -> Text
showDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" . utc2local
  where
    utc2local = utcToLocalTime $ hoursToTimeZone Settings.tz

localDayToUTC :: Day -> UTCTime
localDayToUTC = localTimeToUTC (hoursToTimeZone Settings.tz) . flip LocalTime (TimeOfDay 0 0 0)

toMessageId :: IssueId -> CommentId -> UTCTime -> Text -> Text
toMessageId iid cid time domain = "<" 
                    +++ T.pack (formatTime defaultTimeLocale "%Y%m%d%H%M%S%q" time)
                    +++ "i" +++ showIdCounter iid
                    +++ "c" +++ showIdCounter cid
                    +++ "@" +++ domain
                    +++ ">"

showIdCounter :: Key b e -> Text
showIdCounter (Key (PersistInt64 n)) = showText n
showIdCounter x = showText $ unKey x

showBirthDay :: Profile -> Text
showBirthDay = showText . profileBirth

showEntryYear :: Profile -> Text
showEntryYear = showText . profileEntryYear

showGraduateYear :: Profile -> Text
showGraduateYear u =  case profileGraduateYear u of
  Nothing -> ""
  Just y -> showText y

showTerminated :: Project -> Text
showTerminated p = case projectTerminated p of
  True -> "終了"
  False -> "活動中"

dead :: Project -> Bool
dead = projectTerminated
live :: Project -> Bool
live = not . projectTerminated

showmaybe :: Maybe Text -> Text
showmaybe Nothing  = ""
showmaybe (Just x) = x

showMultilineText :: Text -> Html
showMultilineText = preEscapedText . T.intercalate "<br/>" . T.splitOn "\n"

showShortenText :: Text -> Html
showShortenText = preEscapedText . shorten 26 . safeHead . T.splitOn "\n"
  where
    safeHead :: [Text] -> Text
    safeHead [] = T.empty
    safeHead (s:_) = s
    shorten n s = if T.length s > n then T.take n s +++ ".." else s

-- | Permission System
canView :: User -> User -> Bool
u `canView` t = u == t || permitted
  where
    permitted = 
      case (userRole u, userRole t) of
        (Admin,   _      ) -> True
        (Teacher, _      ) -> True
        (Staff,   _      ) -> True
        (Student, _      ) -> False

canEdit :: User -> User -> Bool
u `canEdit` t = u == t || permitted
  where
    permitted = 
      case (userRole u, userRole t) of
        (Admin,   _      ) -> True
        (Teacher, Student) -> True
        (Teacher, _      ) -> False
        (Staff,   Student) -> True
        (Staff,   _      ) -> False
        (Student, _      ) -> False

canViewTel :: User -> User -> Bool
u `canViewTel` t = u == t || permitted
  where
    permitted =
      case userRole u of
        Admin   -> True
        Teacher -> False
        Staff   -> True
        Student -> False

canEditTel :: User -> User -> Bool
canEditTel = canViewTel

canCreateProject :: User -> Bool
canCreateProject _ = True

canViewHumannetwork :: User -> Bool
canViewHumannetwork u =
  case userRole u of
    Admin -> True
    Teacher -> True
    Staff -> True
    Student -> False

canEditProjectSetting :: User -> Bool
canEditProjectSetting _ = True

canViewUserLocations :: User -> Bool
canViewUserLocations u =
  case userRole u of
    Admin -> True
    Teacher -> True
    Staff -> True
    Student -> False

canSearchUser :: User -> Bool
canSearchUser _ = True

textToOrder :: Text -> SelectOpt (ProjectGeneric backend)
textToOrder "DescProjectUdate" = Desc ProjectUdate
textToOrder "AscProjectUdate" = Asc ProjectUdate
textToOrder "DescProjectCdate" = Desc ProjectCdate
textToOrder "AscProjectCdate" = Asc ProjectCdate
textToOrder "AscProjectName" = Asc ProjectName
textToOrder "DescProjectName" = Desc ProjectName
