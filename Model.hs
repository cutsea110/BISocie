{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Model ( module Model
             , module Model.Fields
             ) where

import ClassyPrelude.Yesod hiding (Reader, try, (<|>), last, many)
import Database.Persist.Quasi hiding (parse)

-- import Yesod.Crud -- FIXME
import Data.Char (isHexDigit)
import Data.List (last)
import Data.Time
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec as P (string)
import qualified Data.Text as T
import Network.Mail.Mime
import Text.Blaze.Internal (preEscapedText)
import Database.Persist.Sql

import qualified Settings (tz)
import BISocie.Helpers.Util
import Model.Fields

type Year = Integer
type Month = Int
type Date = Int

prettyRoleName :: Role -> Text
prettyRoleName Admin = "管理者"
prettyRoleName Staff = "職員"
prettyRoleName Teacher = "教員"
prettyRoleName Student = "在校生/卒業生"

type IssueNo = Int

-- You can define all of your database entities here. You can find more
-- information on persistent and how to declare entities at:
-- http://docs.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith upperCaseSettings "config/models")

-- FIXME Crud
--instance Item User where
--  itemTitle = userInfoOneline

data Effect = Impact | Strike deriving (Show, Eq)
type Color = Text
data ProjectBis = ProjectBis { projectBisId :: ProjectId
                             , projectBisName :: Text
                             , projectBisDescription :: Textarea
                             , projectBisStatuses :: [(Text, Maybe Color, Maybe Effect)]
                             }
toProjectBis :: Entity Project -> ProjectBis
toProjectBis (Entity pid prj) =
  let Right es = parseStatuses $ projectStatuses prj
  in         
   ProjectBis { projectBisId=pid
              , projectBisName=projectName prj
              , projectBisDescription=projectDescription prj
              , projectBisStatuses=es
              }

lookupStatus :: (Eq a) => a -> [(a, b, c)] -> Maybe (a, b, c)
lookupStatus = find . (\x y -> x == fst3 y)

lookupProjectBis :: ProjectId -> [ProjectBis] -> Maybe ProjectBis
lookupProjectBis = find . (\x y -> x == projectBisId y)

parseStatuses :: Textarea -> Either ParseError [(Text, Maybe Color, Maybe Effect)]
parseStatuses (Textarea t) =
    case ps (T.unpack t) of
      Right xs -> Right $ map toData xs
      Left e -> Left e
    where
      ps :: String -> Either ParseError [(String, Maybe String, Maybe Effect)]
      ps s = parse statuses "parse statuses" 
             $ if last s == '\n' then s else s ++ "\n"
      toData :: (String, Maybe String, Maybe Effect) -> (Text, Maybe Color, Maybe Effect)
      toData (s,mc,me) = (T.pack s, fmap T.pack mc, me)

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
        let known = lookup c [ ("赤", "#ffb6c1")
                             , ("緑", "#f0fff0")
                             , ("青", "#add8e6")
                             , ("灰", "#dedfdf")
                             , ("黄", "#f2eeaf")]
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
                             , commentBisContent :: Maybe Textarea
                             , commentBisStatus :: Text
                             , commentBisAutomemo :: Textarea
                             , commentBisAttached :: Maybe (FileHeaderId, FileHeader)
                             , commentBisCheckReader :: Bool
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

toInFilter :: ([a] -> b) -> [a] -> [b]
toInFilter _ [] = []
toInFilter f xs = [f xs]

maybeToFilter :: (a -> b) -> Maybe a -> [b]
maybeToFilter f = fmap f . maybeToList

userInfoOneline :: User -> Text
userInfoOneline u = 
  "[" +++ showPrettyActive u +++ "] " +++ userIdent u +++ " (" +++ userFullName u +++ " , " +++ userRoleName u +++ ")"

showPrettyActive :: User -> Text
showPrettyActive u = if userActive u then "有効" else "無効"

userFullName :: User -> Text
userFullName u = userFamilyName u +++ " " +++ userGivenName u

userFullName' :: User -> Text
userFullName' u = userFamilyName u +++ userGivenName u
    
userRoleName :: User -> Text
userRoleName = prettyRoleName . userRole

userIdentOrName :: Text -> User -> Bool
userIdentOrName q u = T.isInfixOf q (userIdent u) ||
                      T.isInfixOf q (userFullName u) ||
                      T.isInfixOf q (userFullName' u)

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

showLimittime :: Issue -> Text
showLimittime i = fromMaybe "" (fmap showHHMM (issueLimittime i))
  where
    showHHMM = T.pack . formatTime defaultTimeLocale "%H:%M"
    
showLimitdatetime :: Issue -> Text
showLimitdatetime i = showLimitdate i +++ " " +++ showLimittime i

commentLimitDatetime :: Comment -> Maybe UTCTime
commentLimitDatetime = liftM2 day'timeToUTC <$> commentLimitdate <*> commentLimittime

issueLimitDatetime :: Issue -> Maybe UTCTime
issueLimitDatetime = liftM2 day'timeToUTC <$> issueLimitdate <*> issueLimittime

showReminderdate :: Issue -> Text
showReminderdate i = fromMaybe "" (fmap showText (issueReminderdate i))

showMaybeDouble :: Maybe Double -> Text
showMaybeDouble md = fromMaybe "" (fmap showText md)

showDate :: UTCTime -> Text
showDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" . utc2local
  where
    utc2local = utcToLocalTime $ hoursToTimeZone Settings.tz

localDayToUTC :: Day -> UTCTime
localDayToUTC = localTimeToUTC (hoursToTimeZone Settings.tz) . flip LocalTime (TimeOfDay 0 0 0)

day'timeToUTC :: Day -> TimeOfDay -> UTCTime
day'timeToUTC = (localTimeToUTC (hoursToTimeZone Settings.tz) .) . LocalTime

toMessageId :: IssueId -> CommentId -> UTCTime -> Text -> Text
toMessageId iid cid time domain = "<" 
                    +++ T.pack (formatTime defaultTimeLocale "%Y%m%d%H%M%S%q" time)
                    +++ "i" +++ toPathPiece iid
                    +++ "c" +++ toPathPiece cid
                    +++ "@" +++ domain
                    +++ ">"

showBirthDay :: Profile -> Text
showBirthDay = fromMaybe "" . fmap showText . profileBirth

showEntryYear :: Profile -> Text
showEntryYear = fromMaybe "" . fmap showText . profileEntryYear

showGraduateYear :: Profile -> Text
showGraduateYear = fromMaybe "" . fmap showText . profileGraduateYear

showTerminated :: Project -> Text
showTerminated p = if projectTerminated p then "終了" else "活動中"

showCheckReader :: Comment -> Text
showCheckReader c = if commentCheckReader c then "読者確認する" else "読者確認しない"

care :: Comment -> Bool
care = commentCheckReader
nocare :: Comment -> Bool
nocare = not . commentCheckReader

showmaybe :: Maybe Text -> Text
showmaybe = fromMaybe ""

showMultilineText :: Textarea -> Html
showMultilineText = preEscapedText . T.intercalate "<br/>" . T.splitOn "\n" . unTextarea

showShortenText :: Text -> Html
showShortenText = preEscapedText . shorten 26 . safeHead . T.splitOn "\n"
  where
    safeHead :: [Text] -> Text
    safeHead [] = T.empty
    safeHead (s:_) = s
    shorten n s = if T.length s > n then T.take n s +++ ".." else s

instance Eq User where
  u == v = userIdent u == userIdent v

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

canViewUserLocations :: User -> Bool
canViewUserLocations u =
  case userRole u of
    Admin -> True
    Teacher -> True
    Staff -> True
    Student -> False

canSearchUser :: User -> Bool
canSearchUser _ = True

textToOrder :: Text -> SelectOpt Project
textToOrder "DescProjectUdate" = Desc ProjectUdate
textToOrder "AscProjectUdate" = Asc ProjectUdate
textToOrder "DescProjectCdate" = Desc ProjectCdate
textToOrder "AscProjectCdate" = Asc ProjectCdate
textToOrder "AscProjectName" = Asc ProjectName
textToOrder "DescProjectName" = Desc ProjectName

defaultProfile :: Profile
defaultProfile = Profile { profileUser=UserKey (SqlBackendKey 0)
                         , profileBirth=Nothing
                         , profileEntryYear=Nothing
                         , profileGraduateYear=Nothing
                         , profileBranch=Nothing
                         , profileZip=Nothing
                         , profileAddress=Nothing
                         , profileLongitude=Nothing
                         , profileLatitude=Nothing
                         , profileTel=Nothing
                         , profileStation=Nothing
                         , profileHomeZip=Nothing
                         , profileHomeAddress=Nothing
                         , profileHomeLongitude=Nothing
                         , profileHomeLatitude=Nothing
                         , profileHomeTel=Nothing
                         , profileDesiredCourse=Nothing
                         , profileDesiredWorkLocation=Nothing
                         , profileEmployment=Nothing
                         }

selectMailAddresses pid = do
  mapM (p2u.entityVal) =<< selectList [ParticipantsProject ==. pid, ParticipantsReceivemail ==. True] []
  where
    p2u p = do
      u <- get404 $ participantsUser p
      return $ Address (Just $ userFamilyName u `T.append` userGivenName u) (userEmail u)
