{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Handler.Issue where

import Foundation
import Control.Applicative ((<$>),(<*>))
import Control.Monad (when, unless, forM, liftM2)
import Control.Failure
import Control.Monad.Trans.Class
import Data.List (intercalate, intersperse, nub, groupBy)
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.OrdinalDate
import Data.Tuple.HT
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import Network.Mail.Mime
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Text as T
import Text.Blaze (preEscapedText)
import Text.Cassius (cassiusFile)

import BISocie.Helpers.Util
import Settings (mailXHeader, mailMessageIdDomain, fromEmailAddress, issueListLimit, fillGapWidth, pagenateWidth)
import Settings.StaticFiles
import Handler.S3

getCurrentScheduleR :: Handler RepHtml
getCurrentScheduleR = do
  now <- liftIO getCurrentTime
  let (y, m, _) = toGregorian $ utctDay now
  redirect RedirectSeeOther $ ScheduleR y m
  
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
    addWidget $(widgetFile "schedule")
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
    ptcpts <- selectList [ParticipantsUser ==. selfid] []
    let pids = map (participantsProject.snd) ptcpts
    selectList [IssueLimitdate ==. Just day, IssueProject <-. pids ] []
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
          then selectList [ParticipantsProject <-. pids] []
          else do
            ps' <- selectList [ParticipantsUser ==. selfid, ParticipantsProject <-. pids] []
            selectList [ParticipantsProject <-. (map (participantsProject.snd) ps')] []
    selectList [UserId <-. (map (participantsUser.snd) ps)] []
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
            then selectList [ProjectId <-. pids] []
            else do
              ps <- selectList [ParticipantsUser ==. selfid, ParticipantsProject <-. pids] []
              selectList [ProjectId <-. (map (participantsProject.snd) ps)] []
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
             then selectList [] []
             else do
               ps <- selectList [ParticipantsUser ==. selfid] []
               selectList [ProjectId <-. (map (participantsProject . snd) ps)] []
    return $ map toProjectBis prjs'
  defaultLayout $ do
    setTitle "クロスサーチ"
    addCassius $(cassiusFile "templates/issue.cassius")
    addWidget $(widgetFile "crosssearch")

postCrossSearchR :: Handler RepJson
postCrossSearchR = do
  (selfid, self) <- requireAuth
  r <- getUrlRender
  ps <- fmap (fmap readText) $ lookupPostParams "projectid"
  ss <- lookupPostParams "status"
  as <- fmap (fmap (Just . readText)) $ lookupPostParams "assign"
  (lf, lt) <- uncurry (liftM2 (,))
              (fmap (fmap readText) $ lookupPostParam "limitdatefrom",
               fmap (fmap (Just . addDays 1 . readText)) $ lookupPostParam "limitdateto")
  (uf, ut) <- uncurry (liftM2 (,))
              (fmap (fmap (localDayToUTC . readText)) $ lookupPostParam "updatedfrom",
               fmap (fmap (localDayToUTC . addDays 1 . readText)) $ lookupPostParam "updatedto")
  page <- fmap (max 0 . fromMaybe 0 . fmap readText) $ lookupPostParam "page"
  issues <- runDB $ do
    prjs <- if isAdmin self
            then selectList [ProjectId <-. ps] []
            else do
              ps' <- selectList [ParticipantsUser ==. selfid, ParticipantsProject <-. ps] []
              selectList [ProjectId <-. (map (participantsProject.snd) ps')] []
    let (pS, sS, aS) = (toInFilter (IssueProject <-.) $ map fst prjs,
                        toInFilter (IssueStatus <-.) ss, 
                        toInFilter (IssueAssign <-.) as)
        (lF, lT, uF, uT) = (maybeToFilter (IssueLimitdate >=.) lf,
                            maybeToFilter (IssueLimitdate <.) lt,
                            maybeToFilter (IssueUdate >=.) uf,
                            maybeToFilter (IssueUdate <.) ut)
    issues' <- selectList (pS ++ sS ++ aS ++ lF ++ lT ++ uF ++ uT) [Desc IssueUdate, LimitTo issueListLimit, OffsetBy (page*issueListLimit)]
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
              , ("limittime", jsonScalar $ T.unpack $ showLimittime $ issueBisIssue i)
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
    issues <- selectList [IssueProject ==. pid] [Desc IssueUdate]
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
      paging = $(widgetFile "paging")
  defaultLayout $ do
    setTitle $ preEscapedText $ projectBisName prj +++ "案件一覧"
    addCassius $(cassiusFile "templates/issue.cassius")
    addWidget $(widgetFile "issuelist")

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
    addCassius $(cassiusFile "templates/issue.cassius")
    addWidget $(widgetFile "newissue")
      
postNewIssueR :: ProjectId -> Handler RepHtml
postNewIssueR pid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "add" -> addIssueR
    _          -> invalidArgs ["The possible values of '_method' is add"]
    
  where
    addIssueR = do
      (selfid, _) <- requireAuth
      (sbj, cntnt, ldate, ltime, asgn, sts, rdr) <- 
        runInputPost $ (,,,,,,)
        <$> ireq textField "subject"
        <*> iopt textField "content"
        <*> iopt dayField "limitdate"
        <*> iopt timeField "limittime"
        <*> iopt textField "assign"
        <*> ireq textField "status"
        <*> ireq boolField "checkreader"
      Just fi <- lookupFile "attached"
      ino <- runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        unless (p /= Nothing) $ 
          lift $ permissionDenied "あなたはこのプロジェクトに案件を追加することはできません."
        r <- lift getUrlRender
        now <- liftIO getCurrentTime
        update pid [ProjectIssuecounter +=. 1, ProjectUdate =. now]
        prj <- get404 pid
        let ino = projectIssuecounter prj
            asgn' = fromMaybe Nothing (fmap (Just . readText) asgn)
        mfh <- storeAttachedFile selfid fi
        iid <- insert $ Issue { issueProject=pid
                              , issueNumber=ino
                              , issueSubject=sbj
                              , issueAssign=asgn'
                              , issueStatus=sts
                              , issueLimitdate=ldate
                              , issueLimittime=ltime
                              , issueCuser=selfid
                              , issueCdate=now
                              , issueUuser=selfid
                              , issueUdate=now
                              }
        cid <- insert $ Comment { commentProject=pid
                                , commentIssue=iid
                                , commentContent=cntnt
                                , commentAutomemo="init."
                                , commentAssign=asgn'
                                , commentStatus=sts
                                , commentLimitdate=ldate
                                , commentLimittime=ltime
                                , commentAttached=fmap fst mfh
                                , commentCheckReader=rdr
                                , commentCuser=selfid
                                , commentCdate=now
                                }
        emails <- selectMailAddresses pid
        let msgid = toMessageId iid cid now mailMessageIdDomain
        when (isJust cntnt && not (null emails)) $
          liftIO $ renderSendMail Mail
            { mailFrom = fromEmailAddress
            , mailTo = []
            , mailCc = []
            , mailBcc = emails
            , mailHeaders =
                 [ ("Subject", sbj)
                 , ("Message-ID", msgid)
                 , (mailXHeader, showIdCounter pid)
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
                                     ++ T.lines (fromJust cntnt)
                                     ++ [ ""
                                        , "*このメールに直接返信せずにこちらのページから投稿してください。"
                                        , "イシューURL: " +++ r (IssueR pid ino)]
                                     ++ case mfh of
                                       Nothing -> []
                                       Just (fid,_) -> ["添付ファイル: " +++ (r $ AttachedFileR cid fid)]
                     }
                  ]]
          }
        return ino
      redirect RedirectSeeOther $ IssueR pid ino

getIssueR :: ProjectId -> IssueNo -> Handler RepHtml
getIssueR pid ino = do
  (selfid, self) <- requireAuth
  (prj, ptcpts, issue, comments) <- 
    runDB $ do
      p <- getBy $ UniqueParticipants pid selfid
      unless (p /= Nothing || isAdmin self) $ 
        lift $ permissionDenied "あなたはこの案件を閲覧することはできません."
      (iid, issue) <- getBy404 $ UniqueIssue pid ino
      cs <- selectList [CommentIssue ==. iid] [Desc CommentCdate]
      comments <- forM cs $ \(cid, c) -> do
        let uid = commentCuser c
        u <- get404 uid
        mf <- case commentAttached c of
          Nothing -> return Nothing
          Just fid -> do
            f <- get404 fid
            return $ Just (fid, f)
        mreadP <- getBy $ UniqueReader cid selfid
        return $ (cid, 
                  CommentBis { commentBisId=cid
                             , commentBisContent=commentContent c
                             , commentBisStatus=commentStatus c
                             , commentBisAutomemo=commentAutomemo c
                             , commentBisAttached=mf
                             , commentBisCheckReader=commentCheckReader c
                             , commentBisCuser=(uid, u)
                             , commentBisCdate=commentCdate c
                             }
                 ,isJust mreadP)
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
    addWidget $(widgetFile "issue")

postCommentR :: ProjectId -> IssueNo -> Handler RepHtml
postCommentR pid ino = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "add" -> addCommentR
    _          -> invalidArgs ["The possible values of '_method' is add"]
    
  where
    addCommentR = do
      (selfid, _) <- requireAuth
      (cntnt, ldate, ltime, asgn, sts, rdr) <-
        runInputPost $ (,,,,,)
        <$> iopt textField "content"
        <*> iopt dayField "limitdate"
        <*> iopt timeField "limittime"
        <*> iopt textField "assign"
        <*> ireq textField "status"
        <*> ireq boolField "checkreader"
      r <- getUrlRender
      now <- liftIO getCurrentTime
      Just fi <- lookupFile "attached"
      runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        unless (p /= Nothing) $ 
          lift $ permissionDenied "あなたはこのプロジェクトに投稿することはできません."
        (iid, issue) <- getBy404 $ UniqueIssue pid ino
        [(lastCid, lastC)] <- selectList [CommentIssue ==. iid] [Desc CommentCdate, LimitTo 1]
        mfh <- storeAttachedFile selfid fi
        let asgn' = fromMaybe Nothing (fmap (Just . readText) asgn)
            newComment = Comment { commentProject=pid
                                 , commentIssue=iid
                                 , commentContent=cntnt
                                 , commentAutomemo="now writing..."
                                 , commentAssign=asgn'
                                 , commentStatus=sts
                                 , commentLimitdate=ldate
                                 , commentLimittime=ltime
                                 , commentAttached=fmap fst mfh
                                 , commentCheckReader=rdr
                                 , commentCuser=selfid
                                 , commentCdate=now
                                 }
        update iid [ IssueUuser =. selfid
                   , IssueUdate =. now
                   , IssueLimitdate =. ldate
                   , IssueLimittime =. ltime
                   , IssueAssign =. asgn'
                   , IssueStatus =. sts
                   ]
        amemo <- generateAutomemo newComment issue mfh
        when (isNothing cntnt && T.null amemo) $ do
          lift $ invalidArgs ["内容を入力するかイシューの状態を変更してください."]
        cid <- insert $ newComment {commentAutomemo=amemo}
        prj <- get404 pid
        emails <- selectMailAddresses pid
        let msgid = toMessageId iid cid now mailMessageIdDomain
            refid = toMessageId iid lastCid (commentCdate lastC) mailMessageIdDomain
        when (isJust cntnt && not (null emails)) $
          liftIO $ renderSendMail Mail
            { mailFrom = fromEmailAddress
            , mailBcc = emails
            , mailTo = []
            , mailCc = []
            , mailHeaders =
                 [ ("Subject", issueSubject issue)
                 , ("Message-ID", msgid)
                 , ("References", refid)
                 , ("In-Reply-To", refid)
                 , (mailXHeader, showIdCounter pid)
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
                                     ++ T.lines (fromJust cntnt)
                                     ++ [ ""
                                        , "*このメールに直接返信せずにこちらのページから投稿してください。"
                                        , "イシューURL: " +++ r (IssueR pid ino)]
                                     ++ case mfh of
                                       Nothing -> []
                                       Just (fid,_) -> ["添付ファイル: " +++ (r $ AttachedFileR cid fid)]
                     }
                  ]]
          }
      redirect RedirectSeeOther $ IssueR pid ino
        
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
  
postReadCommentR :: CommentId -> Handler RepJson
postReadCommentR cid = do
  (selfid, self) <- requireAuth
  r <- getUrlRender
  _method <- lookupPostParam "_method"
  ret <- runDB $ do
    cmt <- get404 cid
    let pid = commentProject cmt
    p <- getBy $ UniqueParticipants pid selfid
    unless (p /= Nothing || isAdmin self) $
      lift $ permissionDenied "あなたはこのプロジェクトに参加していません."
    case _method of
      Just "add" -> do    
        mr <- getBy $ UniqueReader cid selfid
        case mr of
          Just _ -> return "added"
          Nothing -> do
            now <- liftIO getCurrentTime
            insert $ Reader cid selfid now
            return "added"
      Just "delete" -> do 
        deleteBy $ UniqueReader cid selfid
        return "deleted"
      Nothing -> lift $ invalidArgs ["The possible values of '_method' is add or delete"]
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [ ("status", jsonScalar ret)
                          , ("read", 
                            jsonMap [ ("comment", jsonScalar $ show cid)
                                    , ("reader",
                                       jsonMap [ ("id", jsonScalar $ show selfid)
                                               , ("ident", jsonScalar $ T.unpack $ userIdent self)
                                               , ("name", jsonScalar $ T.unpack $ userFullName self)
                                               , ("uri", jsonScalar $ T.unpack $ r $ ProfileR selfid)
                                               , ("avatar", jsonScalar $ T.unpack $ r $ AvatarImageR selfid)
                                               ])
                                    ])
                          ]

getCommentReadersR :: CommentId -> Handler RepJson
getCommentReadersR cid = do
  (selfid, self) <- requireAuth
  r <- getUrlRender
  readers <- runDB $ do
    cmt <- get404 cid
    let pid = commentProject cmt
    p <- getBy $ UniqueParticipants pid selfid
    unless (p /= Nothing || isAdmin self) $
      lift $ permissionDenied "あなたはこのプロジェクトに参加していません."
    rds' <- selectList [ReaderComment ==. cid] [Asc ReaderCheckdate]
    forM rds' $ \(_, rd') -> do
      let uid' = readerReader rd'
          ra = AvatarImageR uid'
      Just u <- get uid'
      return (uid', u, ra)
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [("readers", jsonList $ map (go r) readers)]
  where
    go r (uid, u, ra) = jsonMap [ ("id", jsonScalar $ show uid)
                                , ("ident", jsonScalar $ T.unpack $ userIdent u)
                                , ("name", jsonScalar $ T.unpack $ userFullName u)
                                , ("uri", jsonScalar $ T.unpack $ r $ ProfileR uid)
                                , ("avatar", jsonScalar $ T.unpack $ r ra)
                                ]

selectParticipants :: (Failure ErrorResponse m, MonadTrans t, PersistBackend t m) =>
     Key t (ProjectGeneric t) -> t m [(Key t User, User)]
selectParticipants pid = do
  mapM (p2u.snd) =<< selectList [ParticipantsProject ==. pid] []
  where
    p2u p = do
      let uid = participantsUser p
      u <- get404 uid
      return (uid, u)

selectMailAddresses :: (Failure ErrorResponse m, MonadTrans t, PersistBackend t m) =>
     Key t (ProjectGeneric t) -> t m [Address]
selectMailAddresses pid = do
  mapM (p2u.snd) =<< selectList [ParticipantsProject ==. pid, ParticipantsReceivemail ==. True] []
  where
    p2u p = do
      u <- get404 $ participantsUser p
      return $ Address (Just $ userFamilyName u `T.append` userGivenName u) (userEmail u)

storeAttachedFile :: (PersistBackend b m, Functor (b m)) => Key backend User -> FileInfo -> b m (Maybe ((Key b (FileHeaderGeneric backend)), T.Text))
storeAttachedFile uid fi = fmap (fmap fst5'snd5) $ upload uid fi
  where
    fst5'snd5 (x,y,_,_,_) = (x,y)

generateAutomemo c i f = do
  let st = if issueStatus i == commentStatus c
           then []
           else ["ステータスを " +++ issueStatus i +++ " から " 
                 +++ commentStatus c +++ " に変更."]
      lm = case (issueLimitDatetime i, commentLimitDatetime c) of
        (Nothing, Nothing) -> []
        (Just x , Nothing) -> ["期限 " +++ showDate x +++ " を期限なしに変更."]
        (Nothing, Just y ) -> ["期限を " +++ showDate y +++ " に設定."]
        (Just x , Just y ) -> if x == y
                              then []
                              else ["期限を " +++ showDate x +++ " から "
                                    +++  showDate y +++ " に変更."]
      af = case f of
        Nothing -> []
        Just (_, fname) -> ["ファイル " +++ fname +++ " を添付."]
  as <- case (issueAssign i, commentAssign c) of
    (Nothing, Nothing) -> return []
    (Just x , Nothing) -> do
      x' <- get404 x
      return ["担当者 " +++ userFullName x' +++ " を担当者なしに変更."]
    (Nothing, Just y ) -> do
      y' <- get404 y
      return ["担当者を " +++ userFullName y' +++ " に設定."]
    (Just x , Just y ) -> do
      x' <- get404 x
      y' <- get404 y
      if x' == y'
        then return []
        else return ["担当者を " +++ userFullName x' +++ " から " +++ 
                     userFullName y' +++ " に変更."]
  return $ T.intercalate "\n" (st ++ as ++ lm ++ af)
