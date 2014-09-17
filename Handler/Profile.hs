{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Handler.Profile where

import Import
import Control.Monad
import Data.Char (ord)
import qualified Data.Text as T
import Data.Time
import Data.Maybe (fromMaybe, fromJust)
import Handler.S3
import Settings (entryStartYear, graduateStartYear)

getProfileR :: UserId -> Handler TypedContent
getProfileR uid = do
  mode <- lookupGetParam "mode"
  case mode of
    Just "v" {- view prof -} -> viewProf
    Just "e" {- edit prof -} -> editProf
    Just _   {-  default  -} -> viewProf
    Nothing  {-  default  -} -> viewProf
  where
    getLab user = 
      if not $ isTeacher user
      then return Nothing
      else do
        ml <- getBy $ UniqueLaboratory uid
        case ml of
          Just (Entity _ l) -> return $ Just l
          Nothing -> return $ Just $ Laboratory { laboratoryHeadResearcher=uid
                                                , laboratoryExtensionNumber=Nothing
                                                , laboratoryRoomNumber=Nothing
                                                , laboratoryCourses=Nothing
                                                }
    getProf user =
      if not $ isStudent user
      then return Nothing 
      else do
        mp <- getBy $ UniqueProfile uid
        case mp of
          Just (Entity _ p) -> return $ Just p
          Nothing -> do
            now <- liftIO getCurrentTime
            let (y, _, _) = toGregorian $ utctDay now
            return $ Just $ Profile { profileUser=uid
                                    , profileBirth=Just $ fromGregorian (y-18) 1 1
                                    , profileEntryYear=Just $ fromInteger y
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

      
    viewProf :: Handler TypedContent
    viewProf = do
      (Entity selfid self) <- requireAuth
      r <- getUrlRender
      let viewprof = (ProfileR uid, [("mode", "v")])
      (user, mprof, mlab) <- 
        runDB $ do
          user <- get404 uid
          mprof <- getProf user
          mlab <- getLab user
          return (user, mprof, mlab)
      selectRep $ do
        provideRep $ defaultLayout $ do
          setTitle "Profile"
          addScriptRemote "https://maps.google.com/maps/api/js?sensor=false"
          $(widgetFile "profile")
          $(widgetFile "viewProfile")
        provideRep $ return $
          object [ "ident" .= userIdent user
                 , "name" .= userFullName user
                 , "uri" .= r (ProfileR uid)
                 , "avatar" .= r (AvatarImageR uid)
                 , "profile" .= fmap fromProf mprof
                 , "lab" .= fmap fromLab mlab
                 ]
    
    editProf :: Handler TypedContent
    editProf = do
      (Entity selfid self) <- requireAuth
      now <- liftIO getCurrentTime
      r <- getUrlRender
      let viewprof = (ProfileR uid, [("mode", "v")])
          (y,_,_) = toGregorian $ utctDay now
      (user, mprof, mlab, eyears, gyears) <-
        runDB $ do
          user <- get404 uid
          unless (self `canEdit` user) $ 
            lift $ permissionDenied "あなたはこのユーザプロファイルを編集することはできません."
          mprof <- getProf user
          mlab <- getLab user
          let eyears = zipWith (\y1 y2 -> (y1==y2, y1)) [Settings.entryStartYear..y+5] $ 
                       repeat (fromMaybe y (join (fmap (fmap toInteger.profileEntryYear) mprof)))
              gyears = zipWith (\y1 y2 -> (Just y1==y2, y1)) [Settings.graduateStartYear..y+5] $
                       repeat (fromMaybe Nothing (fmap (fmap toInteger.profileGraduateYear) mprof))
          return (user, mprof, mlab, eyears, gyears)
      selectRep $ do
        provideRep $ defaultLayout $ do
          setTitle "Profile"
          addScriptRemote "https://maps.google.com/maps/api/js?sensor=false"
          $(widgetFile "profile")
          $(widgetFile "editProfile")
        provideRep $ return $
          object [ "ident" .= userIdent user
                 , "name" .= userFullName user
                 , "uri" .= r (ProfileR uid)
                 , "avatar" .= r (AvatarImageR uid)
                 , "profile" .= fmap fromProf mprof
                 , "lab" .= fmap fromLab mlab
                 ]
      
    fromProf p = object [ "entryYear" .= profileEntryYear p
                        , "branch" .= profileBranch p
                        ]
    fromLab l = object [ "extensionNumber" .= laboratoryExtensionNumber l
                       , "roomNumber" .= laboratoryRoomNumber l
                       ]

postProfileR :: UserId -> Handler Html
postProfileR uid = do
  user <- runDB $ get404 uid
  case userRole user of
    Student -> studentProf
    Teacher -> teacherProf
    _       -> redirect (ProfileR uid, [("mode", "e")] :: [(Text, Text)])
  where
    teacherProf = do
      lab <- runInputPost $ Laboratory uid
        <$> iopt textField "roomnumber"
        <*> iopt textField "extensionnumber"
        <*> iopt textareaField "courses"
      runDB $ do
        mlab <- getBy $ UniqueLaboratory uid
        case mlab of
          Nothing -> insert lab
          Just (Entity lid _) -> replace lid lab >> return lid
      redirect (ProfileR uid, [("mode", "e")] :: [(Text, Text)])
    
    studentProf = do
      prof <- runInputPost $ Profile uid
        <$> iopt dayField "birth"
        <*> iopt intField "entryYear"
        <*> iopt intField "graduateYear"
        <*> iopt textField "branch"
        <*> iopt textField "zip"
        <*> iopt textField "address"
        <*> iopt doubleField "longitude"
        <*> iopt doubleField "latitude"
        <*> iopt textField "tel"
        <*> iopt textField "station"
        <*> iopt textField "homeZip"
        <*> iopt textField "homeAddress"
        <*> iopt doubleField "homeLongitude"
        <*> iopt doubleField "homeLatitude"
        <*> iopt textField "homeTel"
        <*> iopt textField "desiredCourse"
        <*> iopt textField "desiredWorkLocation"
        <*> iopt textField "employment"
      runDB $ do
        mprof <- getBy $ UniqueProfile uid
        case mprof of
          Nothing -> insert prof
          Just (Entity pid _) -> replace pid prof >> return pid
      redirect (ProfileR uid, [("mode", "e")] :: [(Text, Text)])
    
getAvatarImageR :: UserId -> Handler ()
getAvatarImageR uid = do
  (fid, f) <- runDB $ do
    u <- get404 uid
    case userAvatar u of
      Nothing -> lift $ redirect $ StaticR $ sel $ userIdent u
      Just fid -> do
        f <- get404 fid
        return (fid, f)
  getFileR (fileHeaderCreator f) fid
  where
    sel uid' = case T.foldl' (\b c -> b + ord c) 0 uid' `mod` 4 of
      0 -> img_avatar_01_png
      1 -> img_avatar_02_png
      2 -> img_avatar_03_png
      _ -> img_avatar_04_png

postAvatarR :: UserId -> Handler Value
postAvatarR uid = do
  r <- getUrlRender
  mfhid <- lookupPostParam "avatar"
  let avatar = fmap (fromJust . fromPathPiece) mfhid
  runDB $ do
    update uid [UserAvatar =. avatar]
  cacheSeconds 10 -- FIXME
  returnJson $ object [ "uri" .= r (AvatarImageR uid)
                      , "avatar" .= showmaybe mfhid
                      ]
