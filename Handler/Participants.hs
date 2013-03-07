{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Handler.Participants where

import Import
import BISocie.Helpers.Util
import Control.Monad (unless, when, forM)
import Data.Maybe
import Data.Time
import qualified Data.Text as T
import Data.Text (Text)
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
      now <- liftIO getCurrentTime
      runDB $ insert $ Participants pid uid True now
      cacheSeconds 10 -- FIXME
      jsonToRepJson $ object ["participants" .= object
                              [ "project" .= show pid
                              , "user" .= show uid
                              , "status" .= ("added" :: Text)
                              ]
                             ]
    delParticipants :: UserId -> Handler RepJson
    delParticipants uid = do
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
                             ]
    modParticipants :: UserId -> Handler RepJson
    modParticipants uid = do
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
                             ]
