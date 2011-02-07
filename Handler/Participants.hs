{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Participants where

import BISocie
import Control.Monad (when, forM, mplus)

getParticipantsListR :: ProjectId -> Handler RepJson
getParticipantsListR pid = do
  (selfid, self) <- requireAuth
  runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    let viewable = p /= Nothing
    when (not viewable) $ 
      lift $ permissionDenied "あなたはこのプロジェクトに参加していません."
    ps' <- selectList [ParticipantsProjectEq pid] [] 0 0
    us <- forM ps' $ \(id, p) -> do
      let uid' = participantsUser p
      Just u <- get uid'
      return (uid', u, p)
    lift $ do
      cacheSeconds 10 -- FIXME
      jsonToRepJson $ jsonMap [("participants", jsonList $ map go us)]
  where
    go (id, u, p) = jsonMap [ ("id", jsonScalar $ show id)
                            , ("ident", jsonScalar $ userIdent u)
                            , ("name", jsonScalar $ userDisplayName u)
                            , ("role", jsonScalar $ show $ userRole u)
                            , ("prettyrole", jsonScalar $ userRoleName u)
                            , ("receivemail", jsonScalar $ show $ participantsReceivemail p)
                            ]

postParticipantsR :: ProjectId -> Handler RepJson
postParticipantsR pid = do
  _method <- lookupPostParam "_method"
  muid <- lookupPostParam "uid"
  case (_method, muid) of
    (Just "add", Just uid) -> addParticipants $ read uid
    (Just "del", Just uid) -> delParticipants $ read uid
    (_,          Nothing ) -> invalidArgs ["uid query parameter is required."]
    _                      -> invalidArgs ["The possible values of '_method' is modify."]
  where
    addParticipants :: UserId -> Handler RepJson
    addParticipants uid = do
      (selfid, self) <- requireAuth
      runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        let viewable = p /= Nothing
            editable = viewable && userRole self >= Teacher
        when (not editable) $ 
          lift $ permissionDenied "あなたはこのプロジェクトの参加者を編集できません."
        insert $ Participants pid uid True
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
        let viewable = p /= Nothing
            editable = viewable && userRole self >= Teacher
        
        when (not editable) $ 
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
