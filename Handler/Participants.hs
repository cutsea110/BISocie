{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Handler.Participants where

import Import
import Control.Monad (when, forM)
import Data.Time
import Yesod.Auth (requireAuthId)

getParticipantsListR :: ProjectId -> Handler TypedContent
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
  selectRep $ do
    provideRep $ defaultLayout $ do
      now <- liftIO getCurrentTime
      let (y,_,_) = toGregorian $ utctDay now
          eyears = [entryStartYear..y+5]
      setTitle "参加者管理"
      $(widgetFile "participants-list")
    provideRep $ return $ object ["participants" .= array (map (go r) us)]
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

getParticipantsR :: ProjectId -> UserId -> Handler Value
getParticipantsR pid uid = do
  r <- getUrlRender
  (u, p) <- runDB $ (,) <$> get404 uid <*> getBy404 (UniqueParticipants pid uid)
  cacheSeconds 10 -- FIXME
  returnJson $ object [ "id" .= show uid
                      , "ident" .= userIdent u
                      , "uri" .= r (ProfileR uid)
                      , "name" .= userFullName u
                      , "role" .= show (userRole u)
                      , "prettyrole" .= userRoleName u
                      , "receivemail" .= participantsReceivemail (entityVal p)
                      , "participant_uri" .= r (ParticipantsR pid uid)
                      , "avatar" .= r (AvatarImageR uid)
                      ]

putParticipantsR :: ProjectId -> UserId -> Handler Value
putParticipantsR pid uid = do
  r <- getUrlRender
  (st, p) <- runDB $ do
    mp <- getBy $ UniqueParticipants pid uid
    st <- case mp of
      Nothing -> do
        now <- liftIO getCurrentTime
        insert $ Participants pid uid True now
        return ("added" :: Text)
      Just p -> do
        mmail <- lift $ lookupPostParam "mail"
        case mmail of
          Just m -> do
            replace (entityKey p) (entityVal p) { participantsReceivemail = (m == "true") }
            return "modified"
          _ -> lift $ invalidArgs ["The possible values of 'mail' is send,stop."]
    p <- getBy404 $ UniqueParticipants pid uid
    return (st, p)
  cacheSeconds 10 -- FIXME
  returnJson $ object ["participants" .= object
                       [ "project" .= show pid
                       , "user" .= show uid
                       , "status" .= st
                       , "mail" .= participantsReceivemail (entityVal p)
                       ]
                      , "uri" .= r (ParticipantsR pid uid)
                      ]

deleteParticipantsR :: ProjectId -> UserId -> Handler Value
deleteParticipantsR pid uid = do
  r <- getUrlRender
  uid' <- requireAuthId
  runDB $ do
    c <- count [ParticipantsProject ==. pid, ParticipantsUser !=. uid]
    when (uid'==uid && c==0) $
      lift $ permissionDenied "他に参加者が居ないため削除することはできません."
    deleteBy $ UniqueParticipants pid uid
  cacheSeconds 10 -- FIXME
  returnJson $ object ["participants" .= object
                       [ "project" .= show pid
                       , "user" .= show uid
                       , "status" .= ("deleted" :: Text)
                       ]
                      , "uri" .= r (ParticipantsR pid uid)
                      ]
