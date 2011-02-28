{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
{-# LANGUAGE TupleSections #-}
module Handler.Issue where

import BISocie
import Control.Applicative ((<$>),(<*>))
import Control.Monad (unless, forM, mplus, liftM2)
import Data.List (intercalate, intersperse, nub, groupBy)
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.OrdinalDate
import Data.Tuple.HT
import Data.Maybe (fromMaybe)
import Network.Mail.Mime
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding

import BISocie.Helpers.Util
import Settings (mailXHeader, mailMessageIdDomain, issueListLimit, pagenateWidth)
import StaticFiles
import Handler.S3

getCurrentScheduleR :: Handler RepHtml
getCurrentScheduleR = do
  now <- liftIO getCurrentTime
  let (y, m, _) = toGregorian $ utctDay now
  redirect RedirectTemporary $ ScheduleR y m
  
getScheduleR :: Year -> Month -> Handler RepHtml
getScheduleR y m = do
  (selfid, self) <- requireAuth
  now <- liftIO getCurrentTime
  let today = utctDay now
      fday = fromGregorian y m 1
      lday = fromGregorian y m $ gregorianMonthLength y m
      (fweek, _) = mondayStartWeek fday
      (lweek, _) = mondayStartWeek lday
      days = map (map (\(w,d) -> let day = fromWeekDate y w d in (day, classOf day d today)))
             $ groupBy (\x y -> fst x == fst y) [(w, d)| w <- [fweek..lweek], d <- [1..7]]
  defaultLayout $ do
    setTitle $ string $ show y ++ "年" ++ show m ++ "月のスケジュール"
    addCassius $(cassiusFile "schedule")
    addJulius $(juliusFile "schedule")
    addHamlet $(hamletFile "schedule")
  where
    classOf :: Day -> Int -> Day -> String
    classOf day d today = intercalate " " 
                          $ ["schedule-day-cell", toWeekName d] 
                           ++ (if today == day then ["today"] else [])
                           ++ (if currentMonth day then ["currentMonth"] else ["otherMonth"])
    taskUri :: Day -> BISocieRoute
    taskUri d = let (y', m', d') = toGregorian d in TaskR y' m' d'
    showDay :: Day -> String
    showDay = show . thd3 . toGregorian
    currentMonth :: Day -> Bool
    currentMonth d = let (y', m', _) = toGregorian d in y == y' && m == m'
    monthmove n cm = let (y', m', _) = toGregorian $ addGregorianMonthsClip n cm
                     in ScheduleR y' m'
    prevMonth = monthmove (-1)
    nextMonth = monthmove 1
    prevYear = monthmove (-12)
    nextYear = monthmove 12
    toWeekName :: Int -> String
    toWeekName 1 = "Monday"
    toWeekName 2 = "Tuesday"
    toWeekName 3 = "Wednesday"
    toWeekName 4 = "Thursday"
    toWeekName 5 = "Friday"
    toWeekName 6 = "Saturday"
    toWeekName 7 = "Sunday"
    
getTaskR :: Year -> Month -> Date -> Handler RepJson
getTaskR y m d = do
  (selfid, self) <- requireAuth
  r <- getUrlRender
  let day = fromGregorian y m d
  issues <- runDB $ do
    prjs <- viewableProjects (selfid, self)
    let pids = map fst prjs
    selectList [IssueLimitdateEq $ Just day, IssueProjectIn pids ] [] 0 0
  cacheSeconds 10 --FIXME
  jsonToRepJson $ jsonMap [("tasks", jsonList $ map (go r) issues)]
  where
    go r (iid, issue) = jsonMap [ ("id", jsonScalar $ show iid)
                                , ("subject", jsonScalar $ issueSubject issue)
                                , ("uri", jsonScalar $ r $ IssueR (issueProject issue) (issueNumber issue))
                                ]

getAssignListR :: Handler RepJson
getAssignListR = do
  (selfid, self) <- requireAuth
  pids' <- lookupGetParams "projectid"
  let pids = map read pids'
  users <- runDB $ do
    prjs <- viewableProjects' (selfid, self) pids
    let prjids =  map fst prjs
    ptcpts <- selectList [ParticipantsProjectIn prjids] [] 0 0
    let uids = nub $ map (participantsUser.snd) ptcpts
    users'' <- forM uids $ \uid -> do
      u <- get404 uid
      return (uid, u)
    return users''
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [("assigns", jsonList $ map go users)]
  where
    go (uid, u) = jsonMap [ ("uid", jsonScalar $ show uid)
                          , ("name", jsonScalar $ userFullName u)
                          ]

getStatusListR :: Handler RepJson
getStatusListR = do
  (selfid, self) <- requireAuth
  prjids' <- lookupGetParams "projectid"
  let prjids = map read prjids'
  statuses <- runDB $ do
    prjs' <- viewableProjects' (selfid, self) prjids
    prjs <- forM prjs' $ \(pid, prj) -> do
      let (Right es) = parseStatuses $ projectStatuses prj
      return $ ProjectBis { projectBisId=pid
                          , projectBisName=projectName prj
                          , projectBisDescription=projectDescription prj
                          , projectBisStatuses=es
                          }
    return $ nub $ concatMap (map fst3 . projectBisStatuses) prjs
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [("statuses", jsonList $ map jsonScalar statuses)]

getCrossSearchR :: Handler RepHtml
getCrossSearchR = do
  (selfid, self) <- requireAuth
  prjs <- runDB $ do
    prjs' <- viewableProjects (selfid, self)
    forM prjs' $ \(pid, p) -> do
      let (Right es) = parseStatuses $ projectStatuses p
      return $ ProjectBis { projectBisId=pid
                          , projectBisName=projectName p
                          , projectBisDescription=projectDescription p
                          , projectBisStatuses=es
                          }
  defaultLayout $ do
    setTitle $ string "クロスサーチ"
    addCassius $(cassiusFile "issue")
    addJulius $(juliusFile "crosssearch")
    addHamlet $(hamletFile "crosssearch")

postCrossSearchR :: Handler RepJson
postCrossSearchR = do
  (selfid, self) <- requireAuth
  r <- getUrlRender
  ps' <- lookupPostParams "projectid"
  ss' <- lookupPostParams "status"
  as' <- lookupPostParams "assign"
  (lf', lt') <- uncurry (liftM2 (,)) (lookupPostParam "limitdatefrom",
                                      lookupPostParam "limitdateto")
  (uf', ut') <- uncurry (liftM2 (,)) (lookupPostParam "updatedfrom",
                                      lookupPostParam "updatedto")
  page' <- lookupPostParam "page"
  issues <- runDB $ do
    prjs' <- viewableProjects' (selfid, self) $ map read ps'
    prjs <- forM prjs' $ \(pid, prj) -> do
      let (Right es) = parseStatuses $ projectStatuses prj
      return $ ProjectBis { projectBisId=pid
                          , projectBisName=projectName prj
                          , projectBisDescription=projectDescription prj
                          , projectBisStatuses=es
                          }
    let (pS, sS, aS) = (toInFilter IssueProjectIn $ map fst prjs',
                        toInFilter IssueStatusIn ss', 
                        toInFilter IssueAssignIn $ map (Just . read) as')
        (lF, lT, uF, uT) = (maybeToFilter IssueLimitdateGe $ fmap read lf',
                            maybeToFilter IssueLimitdateLt $ fmap (addDays 1 . read) lt',
                            maybeToFilter IssueUdateGe $ fmap (flip UTCTime 0 . read) uf',
                            maybeToFilter IssueUdateLt $ fmap (flip UTCTime 0 . addDays 1 . read) ut')
        page =  max 0 $ fromMaybe 0  $ fmap read $ page'
    issues' <- selectList (pS ++ sS ++ aS ++ lF ++ lT ++ uF ++ uT) [IssueUdateDesc] issueListLimit (page*issueListLimit)
    forM issues' $ \(id, i) -> do
      cu <- get404 $ issueCuser i
      uu <- get404 $ issueUuser i
      mau <- case issueAssign i of
        Nothing -> return Nothing
        Just auid -> get auid
      let (Just prj) = lookupProjectBis (issueProject i) prjs
      return $ (prj, IssueBis id i cu uu mau)
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [("issues", jsonList $ map (go r) issues)]
  where
    colorAndEffect s es = case lookupStatus s es of
      Nothing -> ("", "")
      Just (_, c, e) -> (fromMaybe "" c, fromMaybe "" (fmap show e))
    go r (p, i) = 
      let (c, e) = colorAndEffect (issueStatus $ issueBisIssue i) (projectBisStatuses p)
          projectRoute = IssueListR $ projectBisId p
          issueRoute = IssueR (projectBisId p) (issueNumber $ issueBisIssue i)
      in
      jsonMap [ ("id", jsonScalar $ show $ issueBisId i)
              , ("effect", jsonScalar e)
              , ("color", jsonScalar c)
              , ("project", jsonScalar $ projectBisName p)
              , ("projecturi", jsonScalar $ r $ projectRoute)
              , ("no", jsonScalar $ show $ issueNumber $ issueBisIssue i)
              , ("subject", jsonScalar $ issueSubject $ issueBisIssue i)
              , ("issueuri", jsonScalar $ r $ issueRoute)
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
  page' <- lookupGetParam "page"
  let page = max 0 $ fromMaybe 0  $ fmap read $ page'
  (all, issues'', prj, es) <- runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    unless (p /= Nothing || isAdmin self) $ 
      lift $ permissionDenied "あなたはこのプロジェクトの参加者ではありません."
    prj' <- get404 pid
    let (Right es) = parseStatuses $ projectStatuses prj'
        prj = ProjectBis { projectBisId=pid
                         , projectBisName=projectName prj'
                         , projectBisDescription=projectDescription prj'
                         , projectBisStatuses=es
                         }
    issues <- selectList [IssueProjectEq pid] [IssueUdateDesc] 0 0
    let issues' = take issueListLimit $ drop (page*issueListLimit) issues
    issues'' <- forM issues' $ \issue@(id, i) -> do
      cu <- get404 $ issueCuser i
      uu <- get404 $ issueUuser i
      mau <- case issueAssign i of
        Nothing -> return Nothing
        Just auid -> get auid
      return $ IssueBis id i cu uu mau
    return (issues, issues'', prj, es)
  let issues = zip (concat $ repeat ["odd","even"]::[String]) issues''
      colorOf = \s -> 
        case lookupStatus s es of
          Nothing -> ""
          Just (_, c, _) -> fromMaybe "" c
      effectOf = \s ->
        case lookupStatus s es of
          Nothing -> ""
          Just (_, _, e) -> fromMaybe "" (fmap show e)
      -- pagenate
      maxpage = ceiling (fromIntegral (length all) / fromIntegral issueListLimit) - 1
      prevExist = page > 0
      nextExist = page < maxpage
      prevPage = (IssueListR pid, [("page", show $ max 0 (page-1))])
      nextPage = (IssueListR pid, [("page", show $ max 0 (page+1))])
      pagenate = intersperse [] $  map (map pageN) $ mkPagenate page maxpage pagenateWidth
      pageN = \n -> (n, (IssueListR pid, [("page", show n)]))
      isCurrent = (==page)
      needPaging = maxpage > 0
      inc = (+1)
  defaultLayout $ do
    setTitle $ string $ projectBisName prj ++ "案件一覧"
    addCassius $(cassiusFile "issue")
    addHamlet $(hamletFile "issuelist")

getNewIssueR :: ProjectId -> Handler RepHtml
getNewIssueR pid = do
  (selfid, self) <- requireAuth
  (ptcpts, stss, prj) <- runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    unless (p /= Nothing || isAdmin self) $ 
      lift $ permissionDenied "あなたはこのプロジェクトに案件を追加することはできません."
    prj <- get404 pid
    ptcpts <- selectParticipants pid
    let (Right stss) = parseStatuses $ projectStatuses prj
    return (ptcpts, stss, prj)
  defaultLayout $ do
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
      (sbj, cntnt, ldate, asgn, sts) <- runFormPost' $ (,,,,)
                                        <$> stringInput "subject"
                                        <*> stringInput "content"
                                        <*> maybeDayInput "limitdate"
                                        <*> maybeStringInput "assign"
                                        <*> stringInput "status"
      Just fi <- lookupFile "attached"
      runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        unless (p /= Nothing || isAdmin self) $ 
          lift $ permissionDenied "あなたはこのプロジェクトに案件を追加することはできません."
        r <- lift getUrlRender
        now <- liftIO getCurrentTime
        update pid [ProjectIssuecounterAdd 1, ProjectUdate now]
        prj <- get404 pid
        let ino = projectIssuecounter prj
            asgn' = fromMaybe Nothing (fmap (Just . read) asgn)
        mfhid <- storeAttachedFile selfid fi
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
        let msgid = toMessageId iid cid now mailMessageIdDomain
        liftIO $ renderSendMail Mail
          { mailHeaders =
               [ ("From", "noreply")
               , ("To", intercalate "," $ map (userEmail.snd) ptcpts)
               , ("Subject", sbj)
               , ("Message-ID", msgid)
               , (mailXHeader, show pid)
               ]
          , mailParts = 
                 [[ Part
                     { partType = "text/plain; charset=utf-8"
                     , partEncoding = None
                     , partFilename = Nothing
                     , partHeaders = []
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
  (prj, ptcpts, issue, comments) <- 
    runDB $ do
      p <- getBy $ UniqueParticipants pid selfid
      unless (p /= Nothing || isAdmin self) $ 
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
      return (prj, ptcpts, issue, comments)
  let (Right stss) = parseStatuses $ projectStatuses prj
      isAssign = case issueAssign issue of
        Nothing -> const False
        Just uid -> (==uid)
      isStatus = (==issueStatus issue)
  defaultLayout $ do
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
      Just fi <- lookupFile "attached"
      runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        unless (p /= Nothing || isAdmin self) $ 
          lift $ permissionDenied "あなたはこのプロジェクトに案件を追加することはできません."
        (iid, issue) <- getBy404 $ UniqueIssue pid ino
        [(lastCid, lastC)] <- selectList [CommentIssueEq iid] [CommentCdateDesc] 1 0
        let ldate = limit `mplus` issueLimitdate issue
            asgn' = fromMaybe Nothing (fmap (Just . read) asgn)
        mfhid <- storeAttachedFile selfid fi
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
        let msgid = toMessageId iid cid now mailMessageIdDomain
            refid = toMessageId iid lastCid (commentCdate lastC) mailMessageIdDomain
        liftIO $ renderSendMail Mail
          { mailHeaders =
               [ ("From", "noreply")
               , ("To", intercalate "," $ map (userEmail.snd) ptcpts)
               , ("Subject", issueSubject issue)
               , ("Message-ID", msgid)
               , ("References", refid)
               , ("In-Reply-To", refid)
               , (mailXHeader, show pid)
               ]
          , mailParts = 
                 [[ Part
                     { partType = "text/plain; charset=utf-8"
                     , partEncoding = None
                     , partFilename = Nothing
                     , partHeaders = []
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
  (selfid, self) <- requireAuth
  f <- runDB $ do
    c <- get404 cid
    p <- getBy $ UniqueParticipants (commentProject c) selfid
    unless (p /= Nothing || isAdmin self) $
      lift $ permissionDenied "あなたはこのファイルをダウンロードできません."
    get404 fid
  getFileR (fileHeaderCreator f) fid


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
storeAttachedFile uid fi = do
  mf <- upload uid fi
  case mf of
    Nothing -> return Nothing
    Just (fid, _, _, _, _) -> return $ Just fid
