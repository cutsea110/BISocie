{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.User where

import Control.Monad (unless, forM)
import qualified Data.Text as T

import BISocie

getUserListR :: Handler RepJson
getUserListR = do
  (_, self) <- requireAuth
  r <- getUrlRender
  unless (canSearchUser self) $ 
    permissionDenied "あなたは他のユーザを検索することはできません."
  us <- runDB $ do
    us' <- selectList [UserActive ==. True] []
    forM us' $ \u@(uid, _) -> do
      mp' <- getBy $ UniqueProfile uid
      let ra = AvatarImageR uid
      return (u, mp', ra)
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [("userlist", jsonList $ map (go r) us)]
  where
    go r ((uid, u), mp, ra) = jsonMap [ ("id", jsonScalar $ show uid)
                                      , ("ident", jsonScalar $ T.unpack $ userIdent u)
                                      , ("uri", jsonScalar $ T.unpack $ r $ ProfileR uid)
                                      , ("name", jsonScalar $ T.unpack $ userFullName u)
                                      , ("role", jsonScalar $ show $ userRole u)
                                      , ("prettyrole", jsonScalar $ T.unpack $ userRoleName u)
                                      , ("entryYear", jsonScalar $ T.unpack $ showmaybe $ fmap (showEntryYear.snd) mp)
                                      , ("avatar", jsonScalar $ T.unpack $ r ra)
                                      ]
