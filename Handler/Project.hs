{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Handler.Project where

import Import
import qualified Data.Text as T
import System.Directory
import Text.Blaze.Internal (preEscapedText)

getNewProjectR :: Handler Html
getNewProjectR = do
  u <- requireAuth
  now <- liftIO getCurrentTime
  let inintstatuses = "!未着手#赤\n着手#緑\n完了#灰\n=却下#灰\n保留\n議論\n報告" :: Text
      (y,_,_) = toGregorian $ utctDay now
      eyears = [entryStartYear..y+5]
      help = $(widgetFile "help")
  defaultLayout $ do
    setTitleI MsgCreateNewProject
    $(widgetFile "newproject")
    
postNewProjectR :: Handler Html
postNewProjectR = do
  uid <- requireAuthId
  (name, desc, sts) <- runInputPost $ (,,)
                       <$> ireq textField "name"
                       <*> ireq textareaField "description"
                       <*> ireq textareaField "statuses"
  now <- liftIO getCurrentTime
  pid <- runDB $ do
    pid <- insert $ Project { projectName=name
                            , projectIssuecounter=0
                            , projectDescription=desc
                            , projectStatuses=sts
                            , projectTerminated=False
                            , projectCuser=uid
                            , projectCdate=now
                            , projectUdate=now
                            }
    _ <- insert $ Participants { participantsProject=pid
                               , participantsUser=uid
                               , participantsReceivemail=True
                               , participantsCdate=now
                               }
    return pid
  redirect $ ProjectR pid

getProjectR :: ProjectId -> Handler Html
getProjectR pid = do
  uid <- requireAuthId
  let help = $(widgetFile "help")
  prj <- runDB $ get404 pid
  defaultLayout $ do
    setTitle $ preEscapedText $ projectName prj
    $(widgetFile "project")

postProjectR :: ProjectId -> Handler ()
postProjectR pid = do
  (nm, ds, tm, st) <- runInputPost $ (,,,)
                           <$> ireq textField "name"
                           <*> ireq textareaField "description"
                           <*> ireq boolField "terminated"
                           <*> ireq textareaField "statuses"
  now <- liftIO getCurrentTime
  runDB $ do
    prj <- get404 pid
    let prj' = prj { projectName = nm
                   , projectDescription = ds
                   , projectTerminated = tm
                   , projectStatuses = st
                   , projectUdate = now
                   }
    replace pid prj'
  redirect $ ProjectR pid

deleteProjectR :: ProjectId -> Handler Value
deleteProjectR pid = do
  u <- requireAuth
  deleted <- runDB $ do
    if isAdmin (entityVal u)
      then do
      -- delete participants
      deleteWhere [ParticipantsProject ==. pid]
      -- delete comments
      comments <- selectList [CommentProject ==. pid] []
      deleteWhere [CommentProject ==. pid]
      -- delete & remove files
      let fids = filter (/=Nothing) $ map (commentAttached.entityVal) comments
      forM_ fids $ \(Just fid) -> do 
        f <- get404 fid
        let uid = fileHeaderCreator f
            s3dir' = s3dir </> T.unpack (toPathPiece uid)
            s3fp = s3dir' </> T.unpack (toPathPiece fid)
        delete fid
        liftIO $ removeFile s3fp
      -- delete issues
      deleteWhere [IssueProject ==. pid]
      -- delete project
      delete pid
      return True
      else do
      issues <- selectList [IssueProject ==. pid] []
      if null issues
        then do
        deleteWhere [ParticipantsProject ==. pid]
        delete pid
        return True
        else return False
  cacheSeconds 10 -- FIXME
  returnJson $ 
    if deleted
    then object ["deleted" .= show pid]
    else object ["error" .= ("このプロジェクトは削除できませんでした." :: Text)]
