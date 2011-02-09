{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
module Handler.Issue where

import BISocie
import Control.Applicative ((<$>),(<*>))
import Control.Monad (unless, forM, mplus)
import Data.Time
import Data.Tuple.HT
import Data.Maybe (fromMaybe)

import StaticFiles

getIssueListR :: ProjectId -> Handler RepHtml
getIssueListR pid = do
  (selfid, self) <- requireAuth
  runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    let viewable = p /= Nothing
    unless viewable $ 
      lift $ permissionDenied "あなたはこのプロジェクトの参加者ではありません."
    prj <- get404 pid
    issues' <- selectList [IssueProjectEq pid] [] 0 0
    issues <- forM issues' $ \issue@(id, i) -> do
      cu <- get404 $ issueCuser i
      uu <- get404 $ issueUuser i
      return (issue, cu, uu)
    lift $ defaultLayout $ do
      setTitle $ string $ projectName prj ++ "案件一覧"
      addHamlet $(hamletFile "issuelist")

getNewIssueR :: ProjectId -> Handler RepHtml
getNewIssueR pid = do
  (selfid, self) <- requireAuth
  runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    let addable = p /= Nothing
    unless addable $ 
      lift $ permissionDenied "あなたはこのプロジェクトに案件を追加することはできません."
    prj <- get404 pid
    ptcpts <- do
      ptcpt <- selectList [ParticipantsProjectEq pid] [] 0 0
      forM ptcpt $ \(_, p) -> do
        let uid' = participantsUser p
        u <- get404 uid'
        return (uid', u)
    let stss = lines $ projectStatuses prj
    lift $ defaultLayout $ do
      setTitle $ string "新規案件作成"
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
      (sbj, cntnt, ldate, asgn, sts) <- 
        runFormPost' $ (,,,,)
        <$> stringInput "subject"
        <*> stringInput "content"
        <*> maybeDayInput "limitdate"
        <*> maybeStringInput "assign"
        <*> stringInput "status"
      runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        let addable = p /= Nothing
        unless addable $ 
          lift $ permissionDenied "あなたはこのプロジェクトに案件を追加することはできません."
        now <- liftIO getCurrentTime
        update pid [ProjectIssuecounterAdd 1, ProjectUdate now]
        prj <- get404 pid
        let ino = projectIssuecounter prj
            asgn' = fromMaybe Nothing (fmap (Just . read) asgn)
        iid <- insert $ Issue { issueProject=pid
                              , issueNumber=ino
                              , issueSubject=sbj
                              , issueAssign=asgn'
                              , issueStatus=sts
                              , issueLimitdate=ldate
                              , issueCuser=selfid
                              , issueCdate=now
                              , issueUuser=selfid
                              , issueUdate=now
                              }
        _ <- insert $ Comment { commentProject=pid
                              , commentIssue=iid
                              , commentContent=cntnt
                              , commentAssign=asgn'
                              , commentStatus=sts
                              , commentLimitdate=ldate
                              , commentCuser=selfid
                              , commentCdate=now
                              }
        lift $ redirect RedirectTemporary $ IssueR pid ino

getIssueR :: ProjectId -> IssueNo -> Handler RepHtml
getIssueR pid ino = do
  (selfid, self) <- requireAuth
  runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    let viewable = p /= Nothing
    unless viewable $ 
      lift $ permissionDenied "あなたはこの案件を閲覧することはできません."
    (iid, issue) <- getBy404 $ UniqueIssue pid ino
    cs <- selectList [CommentIssueEq iid] [CommentCdateDesc] 0 0
    comments <- forM cs $ \(_, c) -> do
      Just u <- get $ commentCuser c
      return (u, c)
    prj <- get404 pid
    ptcpts <- do
      ptcpt <- selectList [ParticipantsProjectEq pid] [] 0 0
      forM ptcpt $ \(_, p) -> do
        let uid' = participantsUser p
        u <- get404 uid'
        return (uid', u)
    let stss = lines $ projectStatuses prj
        isAssign = case issueAssign issue of
          Nothing -> const False
          Just uid -> (==uid)
        isStatus = (==issueStatus issue)
    lift $ defaultLayout $ do
      setTitle $ string $ issueSubject issue
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
      (cntnt, limit, asgn, sts) <- 
        runFormPost' $ (,,,)
        <$> stringInput "content"
        <*> maybeDayInput "limitdate"
        <*> maybeStringInput "assign"
        <*> stringInput "status"
      
      now <- liftIO getCurrentTime
      runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        let addable = p /= Nothing
        unless addable $ 
          lift $ permissionDenied "あなたはこのプロジェクトに案件を追加することはできません."
        (iid, issue) <- getBy404 $ UniqueIssue pid ino
        let ldate = limit `mplus` issueLimitdate issue
            asgn' = fromMaybe Nothing (fmap (Just . read) asgn)
        update iid [ IssueUuser selfid
                   , IssueUdate now
                   , IssueLimitdate ldate
                   , IssueAssign asgn'
                   , IssueStatus sts
                   ]
        insert $ Comment { commentProject=pid
                         , commentIssue=iid
                         , commentContent=cntnt
                         , commentAssign=asgn'
                         , commentStatus=sts
                         , commentLimitdate=ldate
                         , commentCuser=selfid
                         , commentCdate=now
                         }
        lift $ redirect RedirectTemporary $ IssueR pid ino
