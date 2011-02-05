{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Participants where

import BISocie
import Control.Monad (when, forM, mplus)

getParticipantsListR :: ProjectId -> Handler RepJson
getParticipantsListR pid = do
  (uid, u) <- requireAuth
  us <- runDB $ do
    ps' <- selectList [ParticipantsProjectEq pid] [] 0 0
    forM ps' $ \(id, p) -> do
      let uid' = participantsUser p
      Just u <- get uid'
      return (uid', u)
  when (lookup uid us == Nothing) $ permissionDenied "あなたはこのプロジェクトに参加していません."
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [("participants", jsonList $ map go us)]
  where
    go (id, u) = jsonMap [ ("id", jsonScalar $ show id)
                         , ("ident", jsonScalar $ userIdent u)
                         , ("name", jsonScalar $ userDisplayName u)
                         , ("role", jsonScalar $ show $ userRole u)
                         , ("prettyrole", jsonScalar $ userRoleName u)
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
      runDB $ insert $ Participants pid uid
      cacheSeconds 10 -- FIXME
      jsonToRepJson $ jsonMap [("participants", 
                                jsonMap [ ("project", jsonScalar $ show pid)
                                        , ("user", jsonScalar $ show uid)
                                        ]
                               )]

    delParticipants :: UserId -> Handler RepJson
    delParticipants uid = do
      runDB $ deleteBy $ UniqueParticipants pid uid
      cacheSeconds 10 -- FIXME
      jsonToRepJson $ jsonMap [("participants", 
                                jsonMap [ ("project", jsonScalar $ show pid)
                                        , ("user", jsonScalar $ show uid)
                                        ]
                               )]
