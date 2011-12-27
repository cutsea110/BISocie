{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Handler.Project where

import Foundation
import Control.Monad (unless, forM_)
import Control.Applicative ((<$>),(<*>))
import Data.Time
import System.Directory
import System.FilePath ((</>))
import Text.Blaze (preEscapedText)
import Text.Cassius (cassiusFile)
import qualified Data.Text as T

import qualified Settings
import Settings.StaticFiles

getNewProjectR :: Handler RepHtml
getNewProjectR = do
  (selfid, self) <- requireAuth
  unless (canCreateProject self) $ 
    permissionDenied "あなたはプロジェクトを作成することはできません."
  now <- liftIO getCurrentTime
  let inintstatuses = "!未開始#赤\n着手#緑\n完了#灰\n=却下#灰\n保留\n議論\n報告" :: String
      (y,_,_) = toGregorian $ utctDay now
      eyears = [Settings.entryStartYear..y+5]
      help = $(widgetFile "help")
  defaultLayout $ do
    setTitle "プロジェクト新規作成"
    addCassius $(cassiusFile "templates/project.cassius")
    addWidget $(widgetFile "newproject")
    
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
      (name, desc, sts) <- runInputPost $ (,,)
                           <$> ireq textField "name"
                           <*> ireq textField "description"
                           <*> ireq textField "statuses"
      now <- liftIO getCurrentTime
      pid <- runDB $ do
        pid <- insert $ Project { projectName=name
                                , projectIssuecounter=0
                                , projectDescription=desc
                                , projectStatuses=sts
                                , projectTerminated=False
                                , projectCuser=selfid
                                , projectCdate=now
                                , projectUdate=now
                                }
        _ <- insert $ Participants { participantsProject=pid 
                                   , participantsUser=selfid 
                                   , participantsReceivemail=True
                                   , participantsCdate=now
                                   }
        return pid
      redirect RedirectTemporary $ ProjectR pid

getProjectR :: ProjectId -> Handler RepHtml
getProjectR pid = do
  (selfid, self) <- requireAuth
  now <- liftIO getCurrentTime
  let (y,_,_) = toGregorian $ utctDay now
      eyears = [Settings.entryStartYear..y+5]
      help = $(widgetFile "help")
  prj <- runDB $ do 
    p <- getBy $ UniqueParticipants pid selfid
    unless (p /= Nothing || isAdmin self) $ 
      lift $ permissionDenied "あなたはこのプロジェクトの参加者ではありません."
    get404 pid
  defaultLayout $ do
    setTitle $ preEscapedText $ projectName prj
    addWidget $(widgetFile "project")

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
    update pid [ ProjectName =. nm
               , ProjectDescription =. ds
               , ProjectStatuses =. st
               , ProjectUdate =. now]
    get404 pid
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [ ("name", jsonScalar $ T.unpack $ projectName prj)
                          , ("description", jsonScalar $ T.unpack $ projectDescription prj)
                          , ("statuses", jsonScalar $ T.unpack $ projectStatuses prj)
                          ]

deleteProjectR :: ProjectId -> Handler RepJson
deleteProjectR pid = do
  (selfid, self) <- requireAuth
  deleted <- runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    unless ((p /= Nothing || isAdmin self) && canEditProjectSetting self) $ 
      lift $ permissionDenied "あなたはこのプロジェクトを削除することはできません."
    if isAdmin self
      then do
      -- delete participants
      deleteWhere [ParticipantsProject ==. pid]
      -- delete comments
      comments <- selectList [CommentProject ==. pid] []
      deleteWhere [CommentProject ==. pid]
      -- delete & remove files
      let fids = filter (/=Nothing) $ map (commentAttached.snd) comments
      forM_ fids $ \(Just fid) -> do 
        f <- get404 fid
        let uid = fileHeaderCreator f
            s3dir = Settings.s3dir </> show uid
            s3fp = s3dir </> show fid
        delete fid
        liftIO $ removeFile s3fp
      -- delete issues
      deleteWhere [IssueProject ==. pid]
      -- delete project
      delete pid
      return True
      else do
      issues <- selectList [IssueProject ==. pid] []
      if issues == [] 
        then do
        deleteWhere [ParticipantsProject ==. pid]
        delete pid
        return True
        else return False
  cacheSeconds 10 -- FIXME
  if deleted
    then jsonToRepJson $ jsonMap [("deleted", jsonScalar $ show pid)]
    else jsonToRepJson $ jsonMap [("error", jsonScalar $ "このプロジェクトは削除できませんでした.")]
