{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
{-# LANGUAGE TupleSections #-}
module Handler.Issue where

import BISocie
import Control.Applicative ((<$>),(<*>))
import Control.Monad (unless, forM, mplus, liftM2)
import Data.List (intercalate)
import Data.Time
import Data.Tuple.HT
import Data.Maybe (fromMaybe)
import Network.Mail.Mime
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding

import qualified Settings (mailXHeader)
import StaticFiles
import Handler.S3

data IssueBis = IssueBis { issueBisId :: IssueId
                         , issueBisIssue :: Issue
                         , issueBisCreator :: User
                         , issueBisUpdator :: User
                         , issueBisAssign :: Maybe User
                         }
data CommentBis = CommentBis { commentBisId :: CommentId
                             , commentBisContent :: String
                             , commentBisStatus :: String
                             , commentBisAttached :: Maybe (FileHeaderId, FileHeader)
                             , commentBisCuser :: (UserId, User)
                             , commentBisCdate :: UTCTime
                             }
                
getIssueListR :: ProjectId -> Handler RepHtml
getIssueListR pid = do
  (selfid, self) <- requireAuth
  runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    let viewable = p /= Nothing
    unless viewable $ 
      lift $ permissionDenied "あなたはこのプロジェクトの参加者ではありません."
    prj <- get404 pid
    issues' <- selectList [IssueProjectEq pid] [IssueNumberDesc] 0 0
    issues'' <- forM issues' $ \issue@(id, i) -> do
      cu <- get404 $ issueCuser i
      uu <- get404 $ issueUuser i
      mau <- case issueAssign i of
        Nothing -> return Nothing
        Just auid -> get auid
      return $ IssueBis id i cu uu mau
    let issues = zip (concat $ repeat ["odd"::String,"even"]) issues''
    lift $ defaultLayout $ do
      setTitle $ string $ projectName prj ++ "案件一覧"
      addCassius $(cassiusFile "issue")
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
    ptcpts <- selectParticipants pid
    let stss = lines $ projectStatuses prj
    lift $ defaultLayout $ do
      setTitle $ string "新規案件作成"
      addCassius $(cassiusFile "issue")
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
        r <- lift getUrlRender
        now <- liftIO getCurrentTime
        update pid [ProjectIssuecounterAdd 1, ProjectUdate now]
        prj <- get404 pid
        let ino = projectIssuecounter prj
            asgn' = fromMaybe Nothing (fmap (Just . read) asgn)
        mfhid <- storeAttachedFile selfid
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
        cid <- insert $ Comment { commentProject=pid
                                , commentIssue=iid
                                , commentContent=cntnt
                                , commentAssign=asgn'
                                , commentStatus=sts
                                , commentLimitdate=ldate
                                , commentAttached=mfhid
                                , commentCuser=selfid
                                , commentCdate=now
                                }
        ptcpts <- selectParticipants pid
        liftIO $ renderSendMail Mail
          { mailHeaders =
               [ ("From", "noreply")
               , ("To", intercalate "," $ map (userEmail.snd) ptcpts)
               , ("Subject", sbj)
               , (Settings.mailXHeader, show pid)
               ]
          , mailParts = 
                 [[ Part
                     { partType = "text/plain; charset=utf-8"
                     , partEncoding = None
                     , partFilename = Nothing
                     , partContent = Data.Text.Lazy.Encoding.encodeUtf8
                                     $ Data.Text.Lazy.pack $ unlines
                                     $ [ "プロジェクト: " ++ projectName prj
                                       , "案件: " ++ sbj
                                       , "ステータス: " ++ sts
                                       , ""
                                       ]
                                     ++ lines cntnt 
                                     ++ [ ""
                                        , "イシュー: " ++ r (IssueR pid ino)]
                                     ++ case mfhid of
                                       Nothing -> []
                                       Just fid -> ["添付ファイル: " ++ (r $ AttachedFileR cid fid)]
                     }
                  ]]
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
    comments <- forM cs $ \(cid, c) -> do
      let uid = commentCuser c
      u <- get404 uid
      mf <- case commentAttached c of
        Nothing -> return Nothing
        Just fid -> do
          f <- get404 fid
          return $ Just (fid, f)
      return $ (cid, CommentBis { commentBisId=cid
                                , commentBisContent=commentContent c
                                , commentBisStatus=commentStatus c
                                , commentBisAttached=mf
                                , commentBisCuser=(uid, u)
                                , commentBisCdate=commentCdate c
                                })
    prj <- get404 pid
    ptcpts <- selectParticipants pid
    let stss = lines $ projectStatuses prj
        isAssign = case issueAssign issue of
          Nothing -> const False
          Just uid -> (==uid)
        isStatus = (==issueStatus issue)
    lift $ defaultLayout $ do
      setTitle $ string $ issueSubject issue
      addCassius $(cassiusFile "issue")
      addJulius $(juliusFile "issue")
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
      r <- getUrlRender
      now <- liftIO getCurrentTime
      runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        let addable = p /= Nothing
        unless addable $ 
          lift $ permissionDenied "あなたはこのプロジェクトに案件を追加することはできません."
        (iid, issue) <- getBy404 $ UniqueIssue pid ino
        let ldate = limit `mplus` issueLimitdate issue
            asgn' = fromMaybe Nothing (fmap (Just . read) asgn)
        mfhid <- storeAttachedFile selfid
        update iid [ IssueUuser selfid
                   , IssueUdate now
                   , IssueLimitdate ldate
                   , IssueAssign asgn'
                   , IssueStatus sts
                   ]
        cid <- insert $ Comment { commentProject=pid
                                , commentIssue=iid
                                , commentContent=cntnt
                                , commentAssign=asgn'
                                , commentStatus=sts
                                , commentLimitdate=ldate
                                , commentAttached=mfhid
                                , commentCuser=selfid
                                , commentCdate=now
                                }
        prj <- get404 pid
        ptcpts <- selectParticipants pid
        liftIO $ renderSendMail Mail
          { mailHeaders =
               [ ("From", "noreply")
               , ("To", intercalate "," $ map (userEmail.snd) ptcpts)
               , ("Subject", issueSubject issue)
               , (Settings.mailXHeader, show pid)
               ]
          , mailParts = 
                 [[ Part
                     { partType = "text/plain; charset=utf-8"
                     , partEncoding = None
                     , partFilename = Nothing
                     , partContent = Data.Text.Lazy.Encoding.encodeUtf8
                                     $ Data.Text.Lazy.pack $ unlines
                                     $ [ "プロジェクト: " ++ projectName prj
                                       , "案件: " ++ issueSubject issue
                                       , "ステータス: " ++ sts
                                       , ""
                                       ]
                                     ++ lines cntnt 
                                     ++ [ ""
                                        , "イシュー: " ++ r (IssueR pid ino)]
                                     ++ case mfhid of
                                       Nothing -> []
                                       Just fid -> ["添付ファイル: " ++ (r $ AttachedFileR cid fid)]
                     }
                  ]]
          }
        lift $ redirect RedirectTemporary $ IssueR pid ino
        
getAttachedFileR :: CommentId -> FileHeaderId -> Handler RepHtml
getAttachedFileR cid fid = do
  (selfid, _) <- requireAuth
  runDB $ do
    c <- get404 cid
    p <- getBy $ UniqueParticipants (commentProject c) selfid
    let viewable = p /= Nothing
    unless viewable $
      lift $ permissionDenied "あなたはこのファイルをダウンロードできません."
    f <- get404 fid
    lift $ getFileR (fileHeaderCreator f) fid


-- | selectParticipants
--  :: (PersistBackend (t m),
--      Control.Failure.Failure ErrorResponse m,
--      Control.Monad.Trans.Class.MonadTrans t) =>
--     ProjectId -> t m [(UserId, User)]
selectParticipants pid = do
  mapM (p2u.snd) =<< selectList [ParticipantsProjectEq pid] [] 0 0
  where
    p2u p = do
      let uid = participantsUser p
      u <- get404 uid
      return (uid, u)

-- | storeAttachedFile
--   :: (Control.Monad.IO.Class.MonadIO m, RequestReader m, PersistBackend m) =>
--      Key User -> m (Maybe (Key FileHeader))
storeAttachedFile uid = do
  Just fi <- lift $ lookupFile "attached"
  mf <- upload uid fi
  case mf of
    Nothing -> return Nothing
    Just (fid, _, _, _, _) -> return $ Just fid
