{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Handler.Participants where

import Import
import BISocie.Helpers.Util
import Control.Monad (unless, when, forM)
import Data.Maybe
import Data.Time
import qualified Data.Text as T

getParticipantsListR :: ProjectId -> Handler RepJson
getParticipantsListR pid = do
  (Entity selfid self) <- requireAuth
  r <- getUrlRender
  us <- runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    unless (isJust p || isAdmin self) $ 
      lift $ permissionDenied "あなたはこのプロジェクトに参加していません."
    ps' <- selectList [ParticipantsProject ==. pid] [Asc ParticipantsCdate]
    forM ps' $ \(Entity _ p') -> do
      let uid' = participantsUser p'
          ra = AvatarImageR uid'
      Just u <- get uid'
      return (uid', u, p', ra)
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ object ["participants" .= array (map (go r) us)]
  where
    go r (uid, u, p, ra) = object [ "id" .= show uid
                                  , "ident" .= userIdent u
                                  , "uri" .= r (ProfileR uid)
                                  , "name" .= userFullName u
                                  , "role" .= show (userRole u)
                                  , "prettyrole" .= userRoleName u
                                  , "receivemail" .= participantsReceivemail p
                                  , "avatar" .= r ra
                                   ]

postParticipantsR :: ProjectId -> Handler RepJson
postParticipantsR pid = do
  _method <- lookupPostParam "_method"
  muid <- lookupPostParam "uid"
  case (_method, muid) of
    (Just "add", Just uid) -> addParticipants $ readText uid
    (Just "del", Just uid) -> delParticipants $ readText uid
    (Just "mod", Just uid) -> modParticipants $ readText uid
    (_,          Nothing ) -> invalidArgs ["uid query parameter is required."]
    _                      -> invalidArgs ["The possible values of '_method' is add,del,mod."]
  where
    addParticipants :: UserId -> Handler RepJson
    addParticipants uid = do
      (Entity selfid self) <- requireAuth
      now <- liftIO getCurrentTime
      runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        unless (isJust p && canEditProjectSetting self) $ 
          lift $ permissionDenied "あなたはこのプロジェクトの参加者を編集できません."
        insert $ Participants pid uid True now
      cacheSeconds 10 -- FIXME
      jsonToRepJson $ object ["participants" .= object
                              [ "project" .= show pid
                              , "user" .= show uid
                              , "status" .= ("added" :: T.Text)
                              ]
                             ]

    delParticipants :: UserId -> Handler RepJson
    delParticipants uid = do
      (Entity selfid self) <- requireAuth
      runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        unless (isJust p && canEditProjectSetting self) $ 
          lift $ permissionDenied "あなたはこのプロジェクトの参加者を編集できません."
        c <- count [ParticipantsProject ==. pid, ParticipantsUser !=. uid]
        when (selfid==uid && c==0) $ 
          lift $ permissionDenied "他に参加者が居ないため削除することはできません."
        deleteBy $ UniqueParticipants pid uid
      cacheSeconds 10 -- FIXME
      jsonToRepJson $ object ["participants" .= object
                              [ "project" .= show pid
                              , "user" .= show uid
                              , "status" .= ("deleted" :: T.Text)
                              ]
                             ]
    modParticipants :: UserId -> Handler RepJson
    modParticipants uid = do
      (Entity selfid self) <- requireAuth
      mmail <- lookupPostParam "mail"
      sendMail <- runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        unless (isJust p && canEditProjectSetting self) $
          lift $ permissionDenied "あなたはこのプロジェクトの参加者を編集できません."
        (Entity ptcptid _) <- getBy404 $ UniqueParticipants pid uid
        case mmail of
          Just "send" -> update ptcptid [ParticipantsReceivemail =. True] >> return True
          Just "stop" -> update ptcptid [ParticipantsReceivemail =. False] >> return False
          _           -> lift $ invalidArgs ["The possible values of 'mail' is send,stop."]
      cacheSeconds 10 -- FIXME
      jsonToRepJson $ object ["participants" .= object
                              [ "project" .= show pid
                              , "user" .= show uid
                              , "status" .= ("modified" :: T.Text)
                              , "mail" .= if sendMail then "send" else ("stop" :: T.Text)
                              ]
                             ]
