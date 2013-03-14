{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Handler.Participants where

import Import
import BISocie.Helpers.Util
import Control.Monad (when, forM)
import Data.Time
import Yesod.Auth (requireAuthId)

getParticipantsListR :: ProjectId -> Handler RepHtmlJson
getParticipantsListR pid = do
  uid <- requireAuthId
  r <- getUrlRender
  (prj, us) <- runDB $ do
    prj' <- get404 pid
    us' <- do
      ps' <- selectList [ParticipantsProject ==. pid] [Asc ParticipantsCdate]
      forM ps' $ \(Entity _ p') -> do
        let uid' = participantsUser p'
            ra = AvatarImageR uid'
        Just u <- get uid'
        return (uid', u, p', ra)
    return (prj', us')
  cacheSeconds 10 -- FIXME
  let widget = do
        now <- liftIO getCurrentTime
        let (y,_,_) = toGregorian $ utctDay now
            eyears = [entryStartYear..y+5]
        setTitle "参加者管理"
        $(widgetFile "participants-list")
      json = object ["participants" .= array (map (go r) us)]
  defaultLayoutJson widget json
  where
    go r (uid, u, p, ra) = object [ "id" .= show uid
                                  , "ident" .= userIdent u
                                  , "uri" .= r (ProfileR uid)
                                  , "name" .= userFullName u
                                  , "role" .= show (userRole u)
                                  , "prettyrole" .= userRoleName u
                                  , "receivemail" .= participantsReceivemail p
                                  , "participant_uri" .= r (ParticipantsR pid uid)
                                  , "avatar" .= r ra
                                   ]

getParticipantsR :: ProjectId -> UserId -> Handler RepJson
getParticipantsR pid uid = do
  r <- getUrlRender
  (u, p) <- runDB $ (,) <$> get404 uid <*> getBy404 (UniqueParticipants pid uid)
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ object [ "id" .= show uid
                         , "ident" .= userIdent u
                         , "uri" .= r (ProfileR uid)
                         , "name" .= userFullName u
                         , "role" .= show (userRole u)
                         , "prettyrole" .= userRoleName u
                         , "receivemail" .= participantsReceivemail (entityVal p)
                         , "participant_uri" .= r (ParticipantsR pid uid)
                         , "avatar" .= r (AvatarImageR uid)
                         ]

postNewParticipantsR :: ProjectId -> Handler RepJson
postNewParticipantsR pid = do
  Just uid' <- lookupPostParam "uid"
  let uid = readText uid'
  r <- getUrlRender
  now <- liftIO getCurrentTime
  runDB $ insert $ Participants pid uid True now
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ object ["participants" .= object
                          [ "project" .= show pid
                          , "user" .= show uid
                          , "status" .= ("added" :: Text)
                          ]
                         , "uri" .= r (ParticipantsR pid uid)
                         ]

putParticipantsR :: ProjectId -> UserId -> Handler RepJson
putParticipantsR pid uid = do
  r <- getUrlRender
  mmail <- lookupPostParam "mail"
  send'stop <- runDB $ do
    (Entity ptcptid _) <- getBy404 $ UniqueParticipants pid uid
    case mmail of
      Just x -> update ptcptid [ParticipantsReceivemail =. (x == "send")] >> return x
      _           -> lift $ invalidArgs ["The possible values of 'mail' is send,stop."]
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ object ["participants" .= object
                          [ "project" .= show pid
                          , "user" .= show uid
                          , "status" .= ("modified" :: Text)
                          , "mail" .= send'stop
                          ]
                         , "uri" .= r (ParticipantsR pid uid)
                         ]

deleteParticipantsR :: ProjectId -> UserId -> Handler RepJson
deleteParticipantsR pid uid = do
  r <- getUrlRender
  uid' <- requireAuthId
  runDB $ do
    c <- count [ParticipantsProject ==. pid, ParticipantsUser !=. uid]
    when (uid'==uid && c==0) $
      lift $ permissionDenied "他に参加者が居ないため削除することはできません."
    deleteBy $ UniqueParticipants pid uid
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ object ["participants" .= object
                          [ "project" .= show pid
                          , "user" .= show uid
                          , "status" .= ("deleted" :: Text)
                          ]
                         , "uri" .= r (ParticipantsR pid uid)
                         ]
