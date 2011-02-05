{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.User where

import BISocie
import Control.Monad (when, forM, mplus)

getUserListR :: Handler RepJson
getUserListR = do
  (uid, u) <- requireAuth
  when (not $ canSearchUser u) $ permissionDenied "あなたは他のユーザを検索することはできません."
  us <- runDB $ selectList [UserActiveEq True] [] 0 0
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [("userlist", jsonList $ map go us)]
  where
    go (uid, u) = jsonMap [ ("id", jsonScalar $ show uid)
                          , ("ident", jsonScalar $ userIdent u)
                          , ("name", jsonScalar $ userDisplayName u)
                          , ("role", jsonScalar $ show $ userRole u)
                          , ("prettyrole", jsonScalar $ userRoleName u)
                          ]
