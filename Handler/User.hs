{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.User where

import BISocie
import Control.Monad (unless, forM)

getUserListR :: Handler RepJson
getUserListR = do
  (_, self) <- requireAuth
  r <- getUrlRender
  unless (canSearchUser self) $ 
    permissionDenied "あなたは他のユーザを検索することはできません."
  us <- runDB $ do
    us' <- selectList [UserActiveEq True] [] 0 0
    forM us' $ \u@(uid, _) -> do
      mp' <- getBy $ UniqueProfile uid
      let ra = AvatarImageR uid
      return (u, mp', ra)
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [("userlist", jsonList $ map (go r) us)]
  where
    go r ((uid, u), mp, ra) = jsonMap [ ("id", jsonScalar $ show uid)
                                      , ("ident", jsonScalar $ userIdent u)
                                      , ("uri", jsonScalar $ r $ ProfileR uid)
                                      , ("name", jsonScalar $ userFullName u)
                                      , ("role", jsonScalar $ show $ userRole u)
                                      , ("prettyrole", jsonScalar $ userRoleName u)
                                      , ("entryYear", jsonScalar $ showmaybe $ fmap (showEntryYear.snd) mp)
                                      , ("avatar", jsonScalar $ r ra)
                                      ]
