{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
module Handler.Project where

import BISocie
import Control.Monad (unless, forM, mplus)
import Data.Time

import StaticFiles

getNewProjectR :: Handler RepHtml
getNewProjectR = do
  (selfid, self) <- requireAuth
  let cancreateproject = userRole self >= Teacher
  unless cancreateproject $ 
    permissionDenied "あなたはプロジェクトを作成することはできません."
  defaultLayout $ do
    setTitle $ string "プロジェクト新規作成"
    addCassius $(cassiusFile "project")
    addHamlet $(hamletFile "newproject")
    
postNewProjectR :: Handler RepHtml
postNewProjectR = do
  (selfid, self) <- requireAuth
  let cancreateproject = userRole self >= Teacher
  unless cancreateproject $ 
    permissionDenied "あなたはプロジェクトを作成することはできません."
  now <- liftIO getCurrentTime
  runDB $ do
    pid <- insert $ Project { projectName=""
                            , projectIssuecounter=0
                            , projectDescription=Nothing
                            , projectStatuses="未開始\n着手\n完了\n却下\n保留\n議論\n報告"
                            , projectCuser=selfid
                            , projectCdate=now
                            , projectUdate=now
                            }
    _ <- insert $ Participants { participantsProject=pid 
                               , participantsUser=selfid 
                               , participantsReceivemail=True
                               }
    lift $ redirect RedirectTemporary $ ProjectR pid

getProjectR :: ProjectId -> Handler RepHtml
getProjectR pid = do
  (selfid, self) <- requireAuth
  runDB $ do 
    p <- getBy $ UniqueParticipants pid selfid
    let viewable = p /= Nothing
        editable = viewable && userRole self >= Teacher
    unless viewable $ 
      lift $ permissionDenied "あなたはこのプロジェクトの参加者ではありません."
    prj <- get404 pid
    now <- liftIO getCurrentTime
    let (y,_,_) = toGregorian $ utctDay now
        eyears = [2000..y+5]
    lift $ defaultLayout $ do
      setTitle $ string $ projectName prj
      addCassius $(cassiusFile "project")
      addJulius $(juliusFile "project")
      addHamlet $(hamletFile "project")


postProjectR :: ProjectId -> Handler RepJson
postProjectR pid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "modify" -> putProjectR pid
    _             -> invalidArgs ["The possible values of '_method' is modify"]

putProjectR :: ProjectId -> Handler RepJson
putProjectR pid = do
  (selfid, self) <- requireAuth
  runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    let viewable = p /= Nothing
        editable = viewable && userRole self >= Teacher
    unless editable $ 
      lift $ permissionDenied "あなたはこのプロジェクトの設定を編集できません."
    prj <- get404 pid
    Just nm <- (lift $ lookupPostParam "name") >>=
               \nm' -> return $ nm' `mplus` (Just $ projectName prj)
    ds <- (lift $ lookupPostParam "description") >>=
          \ds' -> return $ ds' `mplus` projectDescription prj
    Just st <- (lift $ lookupPostParam "statuses") >>=
               \st' -> return $ st' `mplus` (Just $ projectStatuses prj)
    now <- liftIO getCurrentTime
    update pid [ ProjectName nm
               , ProjectDescription ds
               , ProjectStatuses st
               , ProjectUdate now]
    prj <- get404 pid
    lift $ do
      cacheSeconds 10 -- FIXME
      jsonToRepJson $ jsonMap [ ("name", jsonScalar $ projectName prj)
                              , ("description", showMaybeJScalar $ projectDescription prj)
                              , ("statuses", jsonScalar $ projectStatuses prj)
                              ]
    where
      showJScalar :: (Show a) => a -> Json
      showJScalar = jsonScalar . show
      showMaybeJScalar :: Maybe String -> Json
      showMaybeJScalar = jsonScalar . showmaybe
