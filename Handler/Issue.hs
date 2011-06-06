{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.Issue where

import BISocie
import Control.Applicative ((<$>),(<*>))
import Control.Monad (when, unless, forM, mplus, liftM2)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Failure
import Data.List (intercalate, intersperse, nub, groupBy)
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.OrdinalDate
import Data.Tuple.HT
import Data.Maybe (fromMaybe)
import Network.Mail.Mime
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import Data.Text (Text)
import qualified Data.Text as T
import Text.Hamlet (preEscapedText)

import BISocie.Helpers.Util
import Settings (mailXHeader, mailMessageIdDomain, fromEmailAddress, issueListLimit, fillGapWidth, pagenateWidth)
import StaticFiles
import Handler.S3

getCurrentScheduleR :: Handler RepHtml
getCurrentScheduleR = do
  now <- liftIO getCurrentTime
  let (y, m, _) = toGregorian $ utctDay now
  redirect RedirectTemporary $ ScheduleR y m
  
data WeekDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
             deriving (Show, Eq, Ord, Enum)

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
             $ groupBy (\d1 d2 -> fst d1 == fst d2) [(w, d)| w <- [fweek..lweek], d <- [1..7]]
  defaultLayout $ do
    setTitle $ preEscapedText $ showText y +++ "年" +++ showText m +++ "月のスケジュール"
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
    toWeekName = show . toWeekDay
    toWeekDay :: Int -> WeekDay
    toWeekDay n = toEnum (n-1)
    
getTaskR :: Year -> Month -> Date -> Handler RepJson
getTaskR y m d = do
  (selfid, _) <- requireAuth
  r <- getUrlRender
  let day = fromGregorian y m d
  issues <- runDB $ do
    ptcpts <- selectList [ParticipantsUserEq selfid] [] 0 0
    let pids = map (participantsProject.snd) ptcpts
    selectList [IssueLimitdateEq $ Just day, IssueProjectIn pids ] [] 0 0
  cacheSeconds 10 --FIXME
  jsonToRepJson $ jsonMap [("tasks", jsonList $ map (go r) issues)]
  where
    go r (iid, issue) = jsonMap [ ("id", jsonScalar $ show iid)
                                , ("subject", jsonScalar $ T.unpack $ issueSubject issue)
                                , ("uri", jsonScalar $ T.unpack $ r $ IssueR (issueProject issue) (issueNumber issue))
                                ]

getAssignListR :: Handler RepJson
getAssignListR = do
  (selfid, self) <- requireAuth
  pids <- fmap (fmap readText) $ lookupGetParams "projectid"
  users <- runDB $ do
    ps <- if isAdmin self
          then selectList [ParticipantsProjectIn pids] [] 0 0
          else do
            ps' <- selectList [ParticipantsUserEq selfid, ParticipantsProjectIn pids] [] 0 0
            selectList [ParticipantsProjectIn $ map (participantsProject.snd) ps'] [] 0 0
    selectList [UserIdIn $ map (participantsUser.snd) ps] [] 0 0
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [("assigns", jsonList $ map go users)]
  where
    go (uid, u) = jsonMap [ ("uid", jsonScalar $ show uid)
                          , ("name", jsonScalar $ T.unpack $ userFullName u)
                          ]

getStatusListR :: Handler RepJson
getStatusListR = do
  (selfid, self) <- requireAuth
  pids <- fmap (fmap readText) $ lookupGetParams "projectid"
  stss <- runDB $ do
    prjs <- if isAdmin self
            then selectList [ProjectIdIn pids] [] 0 0
            else do
              ps <- selectList [ParticipantsUserEq selfid, ParticipantsProjectIn pids] [] 0 0
              selectList [ProjectIdIn $ map (participantsProject.snd) ps] [] 0 0
    return $ nub $ concatMap (\(_,prj) -> 
                               let (Right es) = parseStatuses $ projectStatuses prj 
                               in map fst3 es) prjs
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [("statuses", jsonList $ map (jsonScalar . T.unpack) stss)]

getCrossSearchR :: Handler RepHtml
getCrossSearchR = do
  (selfid, self) <- requireAuth
  prjs <- runDB $ do
    prjs' <- if isAdmin self
             then selectList [] [] 0 0
             else do
               ps <- selectList [ParticipantsUserEq selfid] [] 0 0
               selectList [ProjectIdIn (map (participantsProject . snd) ps)] [] 0 0
    return $ map toProjectBis prjs'
  defaultLayout $ do
    setTitle "クロスサーチ"
    addCassius $(cassiusFile "issue")
    addJulius $(juliusFile "crosssearch")
    addHamlet $(hamletFile "crosssearch")

postCrossSearchR :: Handler RepJson
postCrossSearchR = do
  (selfid, self) <- requireAuth
  r <- getUrlRender
  ps <- fmap (fmap readText) $ lookupPostParams "projectid"
  ss <- lookupPostParams "status"
  as <- fmap (fmap (Just . readText)) $ lookupPostParams "assign"
  (lf, lt) <- uncurry (liftM2 (,)) 
              (fmap (fmap readText) $ lookupPostParam "limitdatefrom",
               fmap (fmap (addDays 1 . readText)) $ lookupPostParam "limitdateto")
  (uf, ut) <- uncurry (liftM2 (,)) 
              (fmap (fmap (localDayToUTC . readText)) $ lookupPostParam "updatedfrom",
               fmap (fmap (localDayToUTC . addDays 1 . readText)) $ lookupPostParam "updatedto")
  page <- fmap (max 0 . fromMaybe 0 . fmap readText) $ lookupPostParam "page"
  issues <- runDB $ do
    prjs <- if isAdmin self
            then selectList [ProjectIdIn ps] [] 0 0
            else do
              ps' <- selectList [ParticipantsUserEq selfid, ParticipantsProjectIn ps] [] 0 0
              selectList [ProjectIdIn $ map (participantsProject.snd) ps'] [] 0 0
    let (pS, sS, aS) = (toInFilter IssueProjectIn $ map fst prjs, 
                        toInFilter IssueStatusIn ss, 
                        toInFilter IssueAssignIn as)
        (lF, lT, uF, uT) = (maybeToFilter IssueLimitdateGe lf,
                            maybeToFilter IssueLimitdateLt lt,
                            maybeToFilter IssueUdateGe uf,
                            maybeToFilter IssueUdateLt ut)
    issues' <- selectList (pS ++ sS ++ aS ++ lF ++ lT ++ uF ++ uT) [IssueUdateDesc] issueListLimit (page*issueListLimit)
    forM issues' $ \(id', i) -> do
      cu <- get404 $ issueCuser i
      uu <- get404 $ issueUuser i
      mau <- case issueAssign i of
        Nothing -> return Nothing
        Just auid -> get auid
      let (Just prj) = lookupProjectBis (issueProject i) $ map toProjectBis prjs
      return $ (prj, IssueBis id' i cu uu mau)
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
              , ("color", jsonScalar $ T.unpack c)
              , ("project", jsonScalar $ T.unpack $ projectBisName p)
              , ("projecturi", jsonScalar $ T.unpack $ r $ projectRoute)
              , ("no", jsonScalar $ show $ issueNumber $ issueBisIssue i)
              , ("subject", jsonScalar $ T.unpack $ issueSubject $ issueBisIssue i)
              , ("issueuri", jsonScalar $ T.unpack $ r $ issueRoute)
              , ("status", jsonScalar $ T.unpack $ issueStatus $ issueBisIssue i)
              , ("assign", jsonScalar $ T.unpack $ showmaybe $ fmap userFullName $ issueBisAssign i)
              , ("limitdate", jsonScalar $ T.unpack $ showLimitdate $ issueBisIssue i)
              , ("creator", jsonScalar $ T.unpack $ userFullName $ issueBisCreator i)
              , ("updator", jsonScalar $ T.unpack $ userFullName $ issueBisUpdator i)
              , ("updated", jsonScalar $ T.unpack $ showDate $ issueUdate $ issueBisIssue i)
              ]
                
getIssueListR :: ProjectId -> Handler RepHtml
getIssueListR pid = do
  (selfid, self) <- requireAuth
  page' <- lookupGetParam "page"
  let page = max 0 $ fromMaybe 0  $ fmap readText $ page'
  (alliis, issues'', prj, es) <- runDB $ do
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
    issues'' <- forM issues' $ \(id', i) -> do
      cu <- get404 $ issueCuser i
      uu <- get404 $ issueUuser i
      mau <- case issueAssign i of
        Nothing -> return Nothing
        Just auid -> get auid
      return $ IssueBis id' i cu uu mau
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
      maxpage = ceiling (fromIntegral (length alliis) / fromIntegral issueListLimit) - 1
      prevExist = page > 0
      nextExist = page < maxpage
      prevPage = (IssueListR pid, [("page", showText $ max 0 (page-1))])
      nextPage = (IssueListR pid, [("page", showText $ max 0 (page+1))])
      pagenate = intersperse [] $  map (map pageN) $ mkPagenate fillGapWidth pagenateWidth page maxpage
      pageN = \n -> (n, (IssueListR pid, [("page", showText n)]))
      isCurrent = (==page)
      needPaging = maxpage > 0
      inc = (+1)
      colspan = 8
      paging = $(hamletFile "paging")
  defaultLayout $ do
    setTitle $ preEscapedText $ projectBisName prj +++ "案件一覧"
    addCassius $(cassiusFile "issue")
    addHamlet $(hamletFile "issuelist")

getNewIssueR :: ProjectId -> Handler RepHtml
getNewIssueR pid = do
  (selfid, self) <- requireAuth
  (ptcpts, stss, prj) <- runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    unless (p /= Nothing) $ 
      lift $ permissionDenied "あなたはこのプロジェクトに案件を追加することはできません."
    prj <- get404 pid
    ptcpts <- selectParticipants pid
    let (Right stss) = parseStatuses $ projectStatuses prj
    return (ptcpts, stss, prj)
  defaultLayout $ do
    setTitle "新規案件作成"
    addCassius $(cassiusFile "issue")
    addHamlet $(hamletFile "newissue")
      
postNewIssueR :: ProjectId -> Handler RepHtml
postNewIssueR pid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "add" -> addIssueR
    _          -> invalidArgs ["The possible values of '_method' is add"]
    
  where
    addIssueR = do
      (selfid, _) <- requireAuth
      (sbj, cntnt, ldate, asgn, sts) <- runFormPost' $ (,,,,)
                                        <$> stringInput "subject"
                                        <*> stringInput "content"
                                        <*> maybeDayInput "limitdate"
                                        <*> maybeStringInput "assign"
                                        <*> stringInput "status"
      Just fi <- lookupFile "attached"
      runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        unless (p /= Nothing) $ 
          lift $ permissionDenied "あなたはこのプロジェクトに案件を追加することはできません."
        r <- lift getUrlRender
        now <- liftIO getCurrentTime
        update pid [ProjectIssuecounterAdd 1, ProjectUdate now]
        prj <- get404 pid
        let ino = projectIssuecounter prj
            asgn' = fromMaybe Nothing (fmap (Just . readText) asgn)
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
        emails <- selectMailAddresses pid
        let msgid = toMessageId iid cid now mailMessageIdDomain
        when (not $ null emails) $
          liftIO $ renderSendMail Mail
            { mailHeaders =
                 [ ("From", fromEmailAddress)
                 , ("Bcc", T.intercalate "," emails)
                 , ("Subject", sbj)
                 , ("Message-ID", msgid)
                 , (mailXHeader, showText pid)
                 ]
            , mailParts = 
                   [[ Part
                     { partType = "text/plain; charset=utf-8"
                     , partEncoding = None
                     , partFilename = Nothing
                     , partHeaders = []
                     , partContent = Data.Text.Lazy.Encoding.encodeUtf8
                                     $ Data.Text.Lazy.pack $ T.unpack $ T.unlines
                                     $ [ "プロジェクト: " +++ projectName prj
                                       , "案件: " +++ sbj
                                       , "ステータス: " +++ sts
                                       , ""
                                       ]
                                     ++ T.lines cntnt 
                                     ++ [ ""
                                        , "イシュー: " +++ r (IssueR pid ino)]
                                     ++ case mfhid of
                                       Nothing -> []
                                       Just fid -> ["添付ファイル: " +++ (r $ AttachedFileR cid fid)]
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
    setTitle $ preEscapedText $ issueSubject issue
    addCassius $(cassiusFile "issue")
    addJulius $(juliusFile "issue")
    addHamlet $(hamletFile "issue")

postCommentR :: ProjectId -> IssueNo -> Handler RepHtml
postCommentR pid ino = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "add" -> addCommentR
    _          -> invalidArgs ["The possible values of '_method' is add"]
    
  where
    addCommentR = do
      (selfid, _) <- requireAuth
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
        unless (p /= Nothing) $ 
          lift $ permissionDenied "あなたはこのプロジェクトに投稿することはできません."
        (iid, issue) <- getBy404 $ UniqueIssue pid ino
        [(lastCid, lastC)] <- selectList [CommentIssueEq iid] [CommentCdateDesc] 1 0
        let ldate = limit `mplus` issueLimitdate issue
            asgn' = fromMaybe Nothing (fmap (Just . readText) asgn)
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
        emails <- selectMailAddresses pid
        let msgid = toMessageId iid cid now mailMessageIdDomain
            refid = toMessageId iid lastCid (commentCdate lastC) mailMessageIdDomain
        when (not $ null emails) $
          liftIO $ renderSendMail Mail
            { mailHeaders =
                 [ ("From", fromEmailAddress)
                 , ("Bcc", T.intercalate "," emails)
                 , ("Subject", issueSubject issue)
                 , ("Message-ID", msgid)
                 , ("References", refid)
                 , ("In-Reply-To", refid)
                 , (mailXHeader, showText pid)
                 ]
            , mailParts = 
                   [[ Part
                     { partType = "text/plain; charset=utf-8"
                     , partEncoding = None
                     , partFilename = Nothing
                     , partHeaders = []
                     , partContent = Data.Text.Lazy.Encoding.encodeUtf8
                                     $ Data.Text.Lazy.pack $ T.unpack $ T.unlines
                                     $ [ "プロジェクト: " +++ projectName prj
                                       , "案件: " +++ issueSubject issue
                                       , "ステータス: " +++ sts
                                       , ""
                                       ]
                                     ++ T.lines cntnt 
                                     ++ [ ""
                                        , "イシュー: " +++ r (IssueR pid ino)]
                                     ++ case mfhid of
                                       Nothing -> []
                                       Just fid -> ["添付ファイル: " +++ (r $ AttachedFileR cid fid)]
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


selectParticipants :: (PersistBackend (t m),
                       Control.Failure.Failure ErrorResponse m,
                       Control.Monad.Trans.Class.MonadTrans t) =>
                      ProjectId -> t m [(UserId, User)]
selectParticipants pid = do
  mapM (p2u.snd) =<< selectList [ParticipantsProjectEq pid] [] 0 0
  where
    p2u p = do
      let uid = participantsUser p
      u <- get404 uid
      return (uid, u)

selectMailAddresses :: (PersistBackend (t m),
                       Control.Failure.Failure ErrorResponse m,
                       Control.Monad.Trans.Class.MonadTrans t) =>
                      ProjectId -> t m [Text]
selectMailAddresses pid = do
  mapM (p2u.snd) =<< selectList [ParticipantsProjectEq pid, ParticipantsReceivemailEq True] [] 0 0
  where
    p2u p = do
      u <- get404 $ participantsUser p
      return $ userEmail u

storeAttachedFile :: (PersistBackend m, MonadIO m) => UserId -> FileInfo -> m (Maybe FileHeaderId)
storeAttachedFile uid fi = do
  mf <- upload uid fi
  case mf of
    Nothing -> return Nothing
    Just (fid, _, _, _, _) -> return $ Just fid
