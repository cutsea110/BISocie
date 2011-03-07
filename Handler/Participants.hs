{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Participants where

import BISocie
import Control.Monad (unless, when, forM)
import Data.Time

getParticipantsListR :: ProjectId -> Handler RepJson
getParticipantsListR pid = do
  (selfid, self) <- requireAuth
  r <- getUrlRender
  us <- runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    unless (p /= Nothing || isAdmin self) $ 
      lift $ permissionDenied "あなたはこのプロジェクトに参加していません."
    ps' <- selectList [ParticipantsProjectEq pid] [ParticipantsCdateAsc] 0 0
    forM ps' $ \(_, p') -> do
      let uid' = participantsUser p'
          ra = AvatarImageR uid'
      Just u <- get uid'
      return (uid', u, p', ra)
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [("participants", jsonList $ map (go r) us)]
  where
    go r (uid, u, p, ra) = jsonMap [ ("id", jsonScalar $ show uid)
                                   , ("ident", jsonScalar $ userIdent u)
                                   , ("uri", jsonScalar $ r $ ProfileR uid)
                                   , ("name", jsonScalar $ userFullName u)
                                   , ("role", jsonScalar $ show $ userRole u)
                                   , ("prettyrole", jsonScalar $ userRoleName u)
                                   , ("receivemail", jsonScalar $ show $ participantsReceivemail p)
                                   , ("avatar", jsonScalar $ r ra)
                                   ]

postParticipantsR :: ProjectId -> Handler RepJson
postParticipantsR pid = do
  _method <- lookupPostParam "_method"
  muid <- lookupPostParam "uid"
  case (_method, muid) of
    (Just "add", Just uid) -> addParticipants $ read uid
    (Just "del", Just uid) -> delParticipants $ read uid
    (Just "mod", Just uid) -> modParticipants $ read uid
    (_,          Nothing ) -> invalidArgs ["uid query parameter is required."]
    _                      -> invalidArgs ["The possible values of '_method' is add,del,mod."]
  where
    addParticipants :: UserId -> Handler RepJson
    addParticipants uid = do
      (selfid, self) <- requireAuth
      now <- liftIO getCurrentTime
      runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        unless (p /= Nothing && canEditProjectSetting self) $ 
          lift $ permissionDenied "あなたはこのプロジェクトの参加者を編集できません."
        insert $ Participants pid uid True now
      cacheSeconds 10 -- FIXME
      jsonToRepJson $ jsonMap [("participants",
                                jsonMap [ ("project", jsonScalar $ show pid)
                                        , ("user", jsonScalar $ show uid)
                                        , ("status", jsonScalar "added")
                                        ]
                               )]

    delParticipants :: UserId -> Handler RepJson
    delParticipants uid = do
      (selfid, self) <- requireAuth
      runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        unless (p /= Nothing && canEditProjectSetting self) $ 
          lift $ permissionDenied "あなたはこのプロジェクトの参加者を編集できません."
        when (selfid==uid) $ 
          lift $ permissionDenied "自分自身を削除することはできません."
        deleteBy $ UniqueParticipants pid uid
      cacheSeconds 10 -- FIXME
      jsonToRepJson $ jsonMap [("participants",
                                jsonMap [ ("project", jsonScalar $ show pid)
                                        , ("user", jsonScalar $ show uid)
                                        , ("status", jsonScalar "deleted")
                                        ]
                               )]
    modParticipants :: UserId -> Handler RepJson
    modParticipants uid = do
      (selfid, self) <- requireAuth
      mmail <- lookupPostParam "mail"
      liftIO $ putStrLn $ show mmail
      sendMail <- runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        unless (p /= Nothing && canEditProjectSetting self) $
          lift $ permissionDenied "あなたはこのプロジェクトの参加者を編集できません."
        (ptcptid, _) <- getBy404 $ UniqueParticipants pid uid
        case mmail of
          Just "send" -> update ptcptid [ParticipantsReceivemail True] >> return True
          Just "stop" -> update ptcptid [ParticipantsReceivemail False] >> return False
          _           -> lift $ invalidArgs ["The possible values of 'mail' is send,stop."]
      cacheSeconds 10 -- FIXME
      jsonToRepJson $ jsonMap [("participants",
                                jsonMap [ ("project", jsonScalar $ show pid)
                                        , ("user" , jsonScalar $ show uid)
                                        , ("status", jsonScalar "modified")
                                        , ("mail" , jsonScalar $ if sendMail then "send" else "stop")
                                        ]
                               )]
