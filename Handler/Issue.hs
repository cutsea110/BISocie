{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
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
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Text as T
import Text.Blaze (preEscapedText)
import Text.Cassius (cassiusFile)

import BISocie.Helpers.Util
import Settings (mailXHeader, mailMessageIdDomain, fromEmailAddress, issueListLimit, fillGapWidth, pagenateWidth, projectListLimit)
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
    selectList [IssueLimitdate ==. Just day, IssueProject <-. pids ] [Asc IssueLimittime]
  jsonToRepJson $ jsonMap [("tasks", jsonList $ map (go r) issues)]
  where
    go r (iid, issue) = jsonMap [ ("id", jsonScalar $ show iid)
                                , ("subject", jsonScalar $ T.unpack $ issueSubject issue)
                                , ("uri", jsonScalar $ T.unpack $ r $ IssueR (issueProject issue) (issueNumber issue))
                                , ("limittime", jsonScalar $ T.unpack $ showLimittime issue)
                                ]

getProjectListR :: Handler RepJson
getProjectListR = do
  (selfid, self) <- requireAuth
  r <- getUrlRender
  includeTerminated <- fmap isJust $ lookupGetParam "includeterminated"
  project_name <- lookupGetParam "project_name"
  user_ident_or_name <- lookupGetParam "user_ident_or_name"
  let tf = if includeTerminated then [] else [ProjectTerminated ==. False]
  mpage <- fmap (fmap readText) $ lookupGetParam "page"
  ordName <- fmap (fromMaybe "DescProjectUdate") $ lookupGetParam "order"
  let order = [textToOrder ordName]
  prjs' <- runDB $ do
    pats <- case user_ident_or_name of
      Nothing -> selectList [] []
      Just q -> do
        users <- selectList [] []
        let uids = map fst $ filter (userIdentOrName q.snd) users
        selectList [ParticipantsUser <-. uids] []
    let pf = [ProjectId <-. map (participantsProject.snd) pats]
    if isAdmin self
      then selectList (tf++pf) order
      else do
      ps <- selectList [ParticipantsUser ==. selfid] []
      selectList (tf ++ pf ++ [ProjectId <-. (map (participantsProject . snd) ps)]) order
  let allprjs = case project_name of
        Just pn -> filter (T.isInfixOf pn . projectName . snd) prjs'
        Nothing -> prjs'
      pageLength = ceiling (fromIntegral (length allprjs) / fromIntegral projectListLimit)
      prjs = case mpage of
        Nothing -> allprjs
        Just n  -> drop (n*projectListLimit) $ take ((n+1)*projectListLimit) allprjs
  jsonToRepJson $ jsonMap [ ("projects", jsonList $ map (go r) prjs)
                          , ("page", jsonScalar $ fromMaybe "0" $ fmap show mpage)
                          , ("order", jsonScalar $ T.unpack ordName)
                          , ("pageLength", jsonScalar $ show pageLength)
                          ]
  where
    go r (pid, p) = jsonMap [ ("pid", jsonScalar $ show pid)
                            , ("name", jsonScalar $ T.unpack $ projectName p)
                            , ("description", jsonScalar $ T.unpack $ projectDescription p)
                            , ("cdate", jsonScalar $ T.unpack $ showDate $ projectCdate p)
                            , ("udate", jsonScalar $ T.unpack $ showDate $ projectUdate p)
                            , ("issuelistUri", jsonScalar $ T.unpack $ r $ IssueListR pid)
                            , ("projectUri", jsonScalar $ T.unpack $ r $ ProjectR pid)
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
    -- 初回GETなので終了プロジェクトは除外.
    prjs' <- if isAdmin self
             then selectList [ProjectTerminated ==. False] []
             else do
               ps <- selectList [ParticipantsUser ==. selfid] []
               selectList [ ProjectTerminated ==. False
                          , ProjectId <-. (map (participantsProject . snd) ps)] []
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
              (fmap (fmap (Just . readText)) $ lookupPostParam "limitdatefrom",
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
      now <- liftIO getCurrentTime
      issue <- runInputPost $ Issue pid undefined selfid now selfid now
        <$> ireq textField "subject"
        <*> fmap (fmap readText) (iopt textField "assign")
        <*> ireq textField "status"
        <*> iopt dayField "limitdate"
        <*> iopt timeField "limittime"
        <*> iopt dayField "reminderdate"
      comment <- runInputPost $ Comment pid undefined "init." undefined selfid now
        <$> iopt textField "content"
        <*> fmap (fmap readText) (iopt textField "assign")
        <*> ireq textField "status"
        <*> iopt dayField "limitdate"
        <*> iopt timeField "limittime"
        <*> iopt dayField "reminderdate"
        <*> ireq boolField "checkreader"
      Just fi <- lookupFile "attached"
      ino <- runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        unless (p /= Nothing) $ 
          lift $ permissionDenied "あなたはこのプロジェクトに案件を追加することはできません."
        r <- lift getUrlRender
        update pid [ProjectIssuecounter +=. 1, ProjectUdate =. now]
        prj <- get404 pid
        let ino = projectIssuecounter prj
        mfh <- storeAttachedFile selfid fi
        iid <- insert $ issue {issueNumber=ino}
        cid <- insert $ comment {commentIssue=iid, commentAttached=fmap fst mfh}
        emails <- selectMailAddresses pid
        let msgid = toMessageId iid cid now mailMessageIdDomain
            fragment = "#" +++ toSinglePiece cid
        when (isJust (commentContent comment) && not (null emails)) $
          liftIO $ renderSendMail Mail
            { mailFrom = fromEmailAddress
            , mailTo = []
            , mailCc = []
            , mailBcc = emails
            , mailHeaders =
                 [ ("Subject", issueSubject issue)
                 , ("Message-ID", msgid)
                 , (mailXHeader, toSinglePiece pid)
                 ]
            , mailParts = 
                   [[ Part
                     { partType = "text/plain; charset=utf-8"
                     , partEncoding = None
                     , partFilename = Nothing
                     , partHeaders = []
                     , partContent = LE.encodeUtf8 $ L.pack $ T.unpack $ T.unlines
                                     $ [ "プロジェクト: " +++ projectName prj
                                       , "案件: " +++ issueSubject issue
                                       , "ステータス: " +++ issueStatus issue
                                       , ""
                                       ]
                                     ++ T.lines (fromJust (commentContent comment))
                                     ++ [ ""
                                        , "*このメールに直接返信せずにこちらのページから投稿してください。"
                                        , "イシューURL: " +++ r (IssueR pid ino) +++ fragment]
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
      now <- liftIO getCurrentTime
      comment <- runInputPost $ Comment pid undefined "now writing..." undefined selfid now
        <$> iopt textField "content"
        <*> fmap (fmap readText) (iopt textField "assign")
        <*> ireq textField "status"
        <*> iopt dayField "limitdate"
        <*> iopt timeField "limittime"
        <*> iopt dayField "reminderdate"
        <*> ireq boolField "checkreader"
      Just fi <- lookupFile "attached"
      runDB $ do
        p <- getBy $ UniqueParticipants pid selfid
        unless (p /= Nothing) $ 
          lift $ permissionDenied "あなたはこのプロジェクトに投稿することはできません."
        r <- lift getUrlRender
        (iid, issue) <- getBy404 $ UniqueIssue pid ino
        Just (lastCid, lastC) <- selectFirst [CommentIssue ==. iid] [Desc CommentCdate]
        mfh <- storeAttachedFile selfid fi
        amemo <- generateAutomemo comment issue mfh
        replace iid issue { issueUuser = selfid
                          , issueUdate = now
                          , issueLimitdate = commentLimitdate comment
                          , issueLimittime = commentLimittime comment
                          , issueReminderdate = commentReminderdate comment 
                          , issueAssign = commentAssign comment
                          , issueStatus = commentStatus comment
                          }
        when (isNothing (commentContent comment) && T.null amemo) $ do
          lift $ invalidArgs ["内容を入力するかイシューの状態を変更してください."]
        cid <- insert $ comment { commentIssue=iid
                                , commentAttached=fmap fst mfh
                                , commentAutomemo=amemo
                                }
        prj <- get404 pid
        emails <- selectMailAddresses pid
        let msgid = toMessageId iid cid now mailMessageIdDomain
            refid = toMessageId iid lastCid (commentCdate lastC) mailMessageIdDomain
            fragment = "#" +++ toSinglePiece cid
        when (isJust (commentContent comment) && not (null emails)) $
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
                 , (mailXHeader, toSinglePiece pid)
                 ]
            , mailParts = 
                   [[ Part
                     { partType = "text/plain; charset=utf-8"
                     , partEncoding = None
                     , partFilename = Nothing
                     , partHeaders = []
                     , partContent = LE.encodeUtf8 $ L.pack $ T.unpack $ T.unlines
                                     $ [ "プロジェクト: " +++ projectName prj
                                       , "案件: " +++ issueSubject issue
                                       , "ステータス: " +++ issueStatus issue
                                       , ""
                                       ]
                                     ++ T.lines (fromJust (commentContent comment))
                                     ++ [ ""
                                        , "*このメールに直接返信せずにこちらのページから投稿してください。"
                                        , "イシューURL: " +++ r (IssueR pid ino) +++ fragment]
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
            _ <- insert $ Reader cid selfid now
            return "added"
      Just "delete" -> do 
        deleteBy $ UniqueReader cid selfid
        return "deleted"
      _ -> lift $ invalidArgs ["The possible values of '_method' is add or delete"]
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
      rm = case (issueReminderdate i, commentReminderdate c) of
        (Nothing, Nothing) -> []
        (Just x , Nothing) -> ["リマインダメール通知日 " +++ showText x +++ " を通知なしに変更"]
        (Nothing, Just y ) -> ["リマインダメール通知日を " +++ showText y +++ " に設定."]
        (Just x , Just y ) -> if x == y
                              then []
                              else ["リマインダ通知日を " +++ showText x +++ " から "
                                    +++ showText y +++ " に変更."]

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
  return $ T.intercalate "\n" (st ++ as ++ lm ++ rm ++ af)
