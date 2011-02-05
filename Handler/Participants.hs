{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Participants where

import BISocie
import Control.Monad (when, forM, mplus)

getParticipantsR :: ProjectId -> Handler RepJson
getParticipantsR pid = do
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
    go (id, u) = jsonMap [ ("id", jsonScalar $ show $ toInteger id)
                         , ("ident", jsonScalar $ userIdent u)
                         , ("name", jsonScalar $ userDisplayName u)
                         , ("role", jsonScalar $ show $ userRole u)
                         , ("prettyrole", jsonScalar $ userRoleName u)
                         ]

