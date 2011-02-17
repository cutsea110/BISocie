{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
{-# LANGUAGE TupleSections #-}
module Handler.Issue where

import BISocie
import Control.Applicative ((<$>),(<*>))
import Control.Monad (unless, forM, mplus, liftM2)
import Data.List (intercalate, nub)
import Data.Time
import Data.Tuple.HT
import Data.Maybe (fromMaybe)
import Network.Mail.Mime
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding

import qualified Settings (mailXHeader, mailMessageIdDomain)
import StaticFiles
import Handler.S3

getAssignListR :: Handler RepJson
getAssignListR = do
  (selfid, _) <- requireAuth
  pids' <- lookupGetParams "projectid"
  let pids = map read pids'
  runDB $ do
    ptcpts <- selectList [ParticipantsUserEq selfid, ParticipantsProjectIn pids] [] 0 0
    prjids <- forM ptcpts $ \(_, p) -> return $ participantsProject p
    users' <- selectList [ParticipantsProjectIn prjids] [] 0 0
    users'' <- forM users' $ \(_, p) -> do
      let uid = participantsUser p
      u <- get404 uid
      return (uid, u)
    let users = nub users''
    lift $ do
      cacheSeconds 10 -- FIXME
      jsonToRepJson $ jsonMap [("assigns", jsonList $ map go users)]
  where
    go (uid, u) = jsonMap [ ("uid", jsonScalar $ show uid)
                          , ("name", jsonScalar $ userFullName u)
                          ]

getStatusListR :: Handler RepJson
getStatusListR = do
  (selfid, _) <- requireAuth
  prjids' <- lookupGetParams "projectid"
  let prjids = map read prjids'
  runDB $ do
    ptcpts <- selectList [ParticipantsUserEq selfid, ParticipantsProjectIn prjids] [] 0 0
    prjs <- forM ptcpts $ \(_, p) -> do
      let pid = participantsProject p
      prj <- get404 pid
      let (Right es) = statuses $ projectStatuses prj
      return $ ProjectBis { projectBisId=pid
                          , projectBisName=projectName prj
                          , projectBisDescription=projectDescription prj
                          , projectBisStatuses=es
                          }
    let statuses = nub $ concatMap (map fst3 . projectBisStatuses) prjs
    lift $ do
      cacheSeconds 10 -- FIXME
      jsonToRepJson $ jsonMap [("statuses", jsonList $ map jsonScalar statuses)]

getCrossSearchR :: Handler RepHtml
getCrossSearchR = do
  (selfid, self) <- requireAuth
  let cancreateproject = userRole self >= Teacher
  runDB $ do
    ptcpts <- selectList [ParticipantsUserEq selfid] [] 0 0
    prjids <- forM ptcpts $ \(_, ptcpt) -> do
      return $ participantsProject ptcpt
    prjs' <- forM prjids $ \ pid -> do
      p <- get404 pid
      return (pid, p)
    prjs <- forM prjs' $ \(pid, p) -> do
      let (Right es) = statuses $ projectStatuses p
      return $ ProjectBis { projectBisId=pid
                          , projectBisName=projectName p
                          , projectBisDescription=projectDescription p
                          , projectBisStatuses=es
                          }
    lift $ defaultLayout $ do
      setTitle $ string "クロスサーチ"
      addCassius $(cassiusFile "issue")
      addJulius $(juliusFile "crosssearch")
      addHamlet $(hamletFile "crosssearch")

postCrossSearchR :: Handler RepJson
postCrossSearchR = do
  (selfid, self) <- requireAuth
  ps' <- lookupPostParams "projectid"
  ss' <- lookupPostParams "status"
  as' <- lookupPostParams "assign"
  let (pS, sS, aS) = (toInFilter IssueProjectIn $ map read ps', 
                      toInFilter IssueStatusIn ss', 
                      toInFilter IssueAssignIn $ map (Just . read) as')
  runDB $ do
    ptcpts' <- selectList [ParticipantsUserEq selfid] [] 0 0
    prjs <- forM ptcpts' $ \(_, p) -> do
      let pid = participantsProject p
      prj <- get404 pid
      let (Right es) = statuses $ projectStatuses prj
      return (pid, ProjectBis { projectBisId=pid
                              , projectBisName=projectName prj
                              , projectBisDescription=projectDescription prj
                              , projectBisStatuses=es
                              })
    issues' <- selectList (pS ++ sS ++ aS) [IssueUdateDesc] 0 0
    issues <- forM issues' $ \(id, i) -> do
      cu <- get404 $ issueCuser i
      uu <- get404 $ issueUuser i
      mau <- case issueAssign i of
        Nothing -> return Nothing
        Just auid -> get auid
      let (Just prj) = lookup (issueProject i) prjs
      return $ (prj, IssueBis id i cu uu mau)
    lift $ do
      cacheSeconds 10 -- FIXME
      jsonToRepJson $ jsonMap [("issues", jsonList $ map go issues)]
  where
    colorAndEffect s es = case lookupStatus s es of
      Nothing -> ("", "")
      Just (_, c, e) -> (fromMaybe "" c, fromMaybe "" (fmap show e))
    go (p, i) = 
      let (c, e) = colorAndEffect (issueStatus $ issueBisIssue i) (projectBisStatuses p)
      in
      jsonMap [ ("id", jsonScalar $ show $ issueBisId i)
              , ("effect", jsonScalar e)
              , ("color", jsonScalar c)
              , ("project", jsonScalar $ projectBisName p)
              , ("no", jsonScalar $ show $ issueNumber $ issueBisIssue i)
              , ("subject", jsonScalar $ issueSubject $ issueBisIssue i)
              , ("status", jsonScalar $ issueStatus $ issueBisIssue i)
              , ("assign", showMaybeJScalar $ fmap userFullName $ issueBisAssign i)
              , ("limitdate", jsonScalar $ showLimitdate $ issueBisIssue i)
              , ("creator", jsonScalar $ userFullName $ issueBisCreator i)
              , ("updator", jsonScalar $ userFullName $ issueBisUpdator i)
              , ("updated", jsonScalar $ showDate $ issueUdate $ issueBisIssue i)
              ]
    showJScalar :: (Show a) => a -> Json
    showJScalar = jsonScalar . show
    showMaybeJScalar :: Maybe String -> Json
    showMaybeJScalar = jsonScalar . showmaybe
                
getIssueListR :: ProjectId -> Handler RepHtml
getIssueListR pid = do
  (selfid, self) <- requireAuth
  runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    let viewable = p /= Nothing
    unless viewable $ 
      lift $ permissionDenied "あなたはこのプロジェクトの参加者ではありません."
    prj' <- get404 pid
    let (Right es) = statuses $ projectStatuses prj'
        prj = ProjectBis { projectBisId=pid
                         , projectBisName=projectName prj'
                         , projectBisDescription=projectDescription prj'
                         , projectBisStatuses=es
                         }
    issues' <- selectList [IssueProjectEq pid] [IssueNumberDesc] 0 0
    issues'' <- forM issues' $ \issue@(id, i) -> do
      cu <- get404 $ issueCuser i
      uu <- get404 $ issueUuser i
      mau <- case issueAssign i of
        Nothing -> return Nothing
        Just auid -> get auid
      return $ IssueBis id i cu uu mau
    let issues = zip (concat $ repeat ["odd"::String,"even"]) issues''
        colorOf = \s -> 
          case lookupStatus s es of
            Nothing -> ""
            Just (_, c, _) -> fromMaybe "" c
        effectOf = \s ->
          case lookupStatus s es of
            Nothing -> ""
            Just (_, _, e) -> fromMaybe "" (fmap show e)
    lift $ defaultLayout $ do
      setTitle $ string $ projectBisName prj ++ "案件一覧"
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
    let (Right stss) = statuses $ projectStatuses prj
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
        let msgid = toMessageId iid cid now Settings.mailMessageIdDomain
        liftIO $ renderSendMail Mail
          { mailHeaders =
               [ ("From", "noreply")
               , ("To", intercalate "," $ map (userEmail.snd) ptcpts)
               , ("Subject", sbj)
               , ("Message-ID", msgid)
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
    let (Right stss) = statuses $ projectStatuses prj
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
        [(lastCid, lastC)] <- selectList [CommentIssueEq iid] [CommentCdateDesc] 1 0
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
        let msgid = toMessageId iid cid now Settings.mailMessageIdDomain
            refid = toMessageId iid lastCid (commentCdate lastC) Settings.mailMessageIdDomain
        liftIO $ renderSendMail Mail
          { mailHeaders =
               [ ("From", "noreply")
               , ("To", intercalate "," $ map (userEmail.snd) ptcpts)
               , ("Subject", issueSubject issue)
               , ("Message-ID", msgid)
               , ("References", refid)
               , ("In-Reply-To", refid)
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
