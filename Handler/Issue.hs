{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
module Handler.Issue where

import BISocie
import Control.Applicative ((<$>),(<*>))
import Control.Monad (when, forM, mplus)
import Data.Time
import Data.Tuple.HT

import StaticFiles

getIssueListR :: ProjectId -> Handler RepHtml
getIssueListR pid = do
  (selfid, self) <- requireAuth
  (prj, issues) <- runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    let viewable = p /= Nothing
    when (not viewable) $ 
      lift $ permissionDenied "あなたはこのプロジェクトの参加者ではありません."
    prj <- get404 pid
    issues' <- selectList [IssueProjectEq pid] [] 0 0
    issues <- forM issues' $ \issue@(id, i) -> do
      Just cu <- get $ issueCuser i
      Just uu <- get $ issueUuser i
      return (issue, cu, uu)
    return (prj, issues)
  defaultLayout $ do
    setTitle $ string $ projectName prj ++ "案件一覧"
    addHamlet $(hamletFile "issuelist")

getNewIssueR :: ProjectId -> Handler RepHtml
getNewIssueR pid = do
  (selfid, self) <- requireAuth
  runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    let addable = p /= Nothing
    when (not addable) $ 
      lift $ permissionDenied "あなたはこのプロジェクトに案件を追加することはできません."
  defaultLayout $ do
    addHamlet $(hamletFile "newissue")

postNewIssueR :: ProjectId -> Handler RepHtml
postNewIssueR pid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "add" -> addIssueR pid
    _          -> invalidArgs ["The possible values of '_method' is add"]
    
  where
    addIssueR :: ProjectId -> Handler RepHtml
    addIssueR pid = do
      (selfid, self) <- requireAuth
      (sbj, cntnt) <- runFormPost' $ (,)
                      <$> stringInput "subject"
                      <*> stringInput "content"
      now <- liftIO getCurrentTime
      ino <- runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        let addable = p /= Nothing
        when (not addable) $ 
          lift $ permissionDenied "あなたはこのプロジェクトに案件を追加することはできません."
        update pid [ProjectIssuecounterAdd 1, ProjectUdate now]
        prj <- get404 pid
        let ino = projectIssuecounter prj
        iid <- insert $ initIssue selfid pid ino sbj now
        _ <- insert $ initComment selfid pid iid cntnt now
        return ino
      redirect RedirectTemporary $ IssueR pid ino

getIssueR :: ProjectId -> IssueNo -> Handler RepHtml
getIssueR pid ino = do
  (selfid, self) <- requireAuth
  (issue, comments) <- runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    let viewable = p /= Nothing
    when (not viewable) $ 
      lift $ permissionDenied "あなたはこの案件を閲覧することはできません."
    issue@(iid, _) <- getBy404 $ UniqueIssue pid ino
    cs <- selectList [CommentIssueEq iid] [CommentCdateDesc] 0 0
    comments <- forM cs $ \(_, c) -> do
      Just u <- get $ commentCuser c
      return (u, c)
    return (issue, comments)
  defaultLayout $ do
    addHamlet $(hamletFile "issue")

postCommentR :: ProjectId -> IssueNo -> Handler RepHtml
postCommentR pid ino = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "add" -> addCommentR pid ino
    _          -> invalidArgs ["The possible values of '_method' is add"]
    
  where
    addCommentR :: ProjectId -> IssueNo -> Handler RepHtml
    addCommentR pid ino = do
      (selfid, self) <- requireAuth
      cntnt <- runFormPost' $ stringInput "content"
      now <- liftIO getCurrentTime
      runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        let addable = p /= Nothing
        when (not addable) $ 
          lift $ permissionDenied "あなたはこのプロジェクトに案件を追加することはできません."
        (iid, _) <- getBy404 $ UniqueIssue pid ino
        update iid [IssueUuser selfid, IssueUdate now]
        insert $ initComment selfid pid iid cntnt now
      redirect RedirectTemporary $ IssueR pid ino
