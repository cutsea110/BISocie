{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
module Handler.Project where

import BISocie
import Control.Monad (unless, forM, mplus)
import Control.Applicative ((<$>),(<*>))
import Data.Time

import qualified Settings
import StaticFiles

getNewProjectR :: Handler RepHtml
getNewProjectR = do
  (selfid, self) <- requireAuth
  unless (canCreateProject self) $ 
    permissionDenied "あなたはプロジェクトを作成することはできません."
  now <- liftIO getCurrentTime
  let inintstatuses = "!未開始#赤\n着手#緑\n完了#灰\n=却下#灰\n保留\n議論\n報告" :: String
      (y,_,_) = toGregorian $ utctDay now
      eyears = [Settings.entryStartYear..y+5]
      help = $(Settings.hamletFile "help")
  defaultLayout $ do
    setTitle $ string "プロジェクト新規作成"
    addCassius $(cassiusFile "project")
    addJulius $(juliusFile "help")
    addJulius $(juliusFile "newproject")
    addHamlet $(hamletFile "newproject")
    
postNewProjectR :: Handler RepHtml
postNewProjectR = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "create" -> createProject
    _             -> invalidArgs ["The possible values of '_method' is create"]
  where
    createProject :: Handler RepHtml
    createProject = do
      (selfid, self) <- requireAuth
      unless (canCreateProject self) $ 
        permissionDenied "あなたはプロジェクトを作成することはできません."
      (name, desc, sts) <- runFormPost'$ (,,)
                           <$> stringInput "name"
                           <*> stringInput "description"
                           <*> stringInput "statuses"
      now <- liftIO getCurrentTime
      runDB $ do
        pid <- insert $ Project { projectName=name
                                , projectIssuecounter=0
                                , projectDescription=desc
                                , projectStatuses=sts
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
  now <- liftIO getCurrentTime
  let (y,_,_) = toGregorian $ utctDay now
      eyears = [Settings.entryStartYear..y+5]
      help = $(Settings.hamletFile "help")
  prj <- runDB $ do 
    p <- getBy $ UniqueParticipants pid selfid
    unless (p /= Nothing) $ 
      lift $ permissionDenied "あなたはこのプロジェクトの参加者ではありません."
    get404 pid
  defaultLayout $ do
    setTitle $ string $ projectName prj
    addCassius $(cassiusFile "project")
    addJulius $(juliusFile "help")
    addJulius $(juliusFile "project")
    addHamlet $(hamletFile "project")


postProjectR :: ProjectId -> Handler RepJson
postProjectR pid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "modify" -> putProjectR pid
    Just "delete" -> deleteProjectR pid
    _             -> invalidArgs ["The possible values of '_method' are modify or delete"]

putProjectR :: ProjectId -> Handler RepJson
putProjectR pid = do
  (selfid, self) <- requireAuth
  nm' <- lookupPostParam "name"
  ds' <- lookupPostParam "description"
  st' <- lookupPostParam "statuses"
  now <- liftIO getCurrentTime

  prj <- runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    unless (p /= Nothing && canEditProjectSetting self) $ 
      lift $ permissionDenied "あなたはこのプロジェクトの設定を編集できません."
    prj <- get404 pid
    Just nm <- case nm' of
        Nothing -> return $ Just $ projectName prj
        Just "" -> lift $ invalidArgs ["プロジェクト名は入力必須項目です."]
        Just nm'' -> return $ Just nm''
    Just ds <- case ds' of
        Nothing -> return $ Just $ projectDescription prj
        Just "" -> lift $ invalidArgs ["概要は入力必須項目です."]
        Just ds'' -> return $ Just ds''
    Just st <- case st' of
        Nothing -> return $ Just $ projectStatuses prj
        Just "" -> lift $ invalidArgs ["ステータスは入力必須項目です."]
        Just st'' -> return $ Just st''
    update pid [ ProjectName nm
               , ProjectDescription ds
               , ProjectStatuses st
               , ProjectUdate now]
    get404 pid
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [ ("name", jsonScalar $ projectName prj)
                          , ("description", jsonScalar $ projectDescription prj)
                          , ("statuses", jsonScalar $ projectStatuses prj)
                          ]
  where
    showJScalar :: (Show a) => a -> Json
    showJScalar = jsonScalar . show
    showMaybeJScalar :: Maybe String -> Json
    showMaybeJScalar = jsonScalar . showmaybe

deleteProjectR :: ProjectId -> Handler RepJson
deleteProjectR pid = do
  (selfid, self) <- requireAuth
  deleted <- runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    unless (p /= Nothing && canEditProjectSetting self) $ 
      lift $ permissionDenied "あなたはこのプロジェクトを削除することはできません."
    issues <- selectList [IssueProjectEq pid] [] 1 0
    if issues == [] 
      then do
      deleteWhere [ParticipantsProjectEq pid]
      delete pid
      return True
      else return False
  cacheSeconds 10 -- FIXME
  if deleted
    then jsonToRepJson $ jsonMap [("deleted", jsonScalar $ show pid)]
    else jsonToRepJson $ jsonMap [("error", jsonScalar $ "このプロジェクトは削除できませんでした.")]
