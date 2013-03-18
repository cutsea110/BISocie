{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Handler.Issue
       ( getCurrentScheduleR
       , getScheduleR
       , getTaskR
       , postExportCsvR
       , getProjectListR
       , getAssignListR
       , getStatusListR
       , getCrossSearchR
       , postCrossSearchR
       , getIssueListR
       , getNewIssueR
       , postNewIssueR
       , getIssueR
       , postCommentR
       , getAttachedFileR
       , postReadCommentR
       , deleteReadCommentR
       , getCommentReadersR
       ) where

import Import
import BISocie.Helpers.Util
import Control.Monad (when, forM, liftM2)
import Data.Function (on)
import Data.List (intercalate, intersperse, nub, groupBy)
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Text as T
import Handler.S3
import Network.Mail.Mime
import Text.Blaze.Internal (preEscapedText)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (stext)
import Yesod.Auth (requireAuthId)

getCurrentScheduleR :: Handler RepHtml
getCurrentScheduleR = do
  today <- liftIO $ fmap utctDay getCurrentTime
  let (y, m, _) = toGregorian today
  redirect $ ScheduleR y m
  
data WeekDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
             deriving (Show, Eq, Ord, Enum)

getScheduleR :: Year -> Month -> Handler RepHtml
getScheduleR y m = do
  u <- requireAuth
  today <- liftIO $ fmap utctDay getCurrentTime
  let days = map (map (ywd2cell today))
             $ groupBy ((==) `on` snd3)
             $ [toWeekDate d | d <- [fromWeekDate fy fm 1 .. fromWeekDate ly lm 7]]
  defaultLayout $ do
    setTitle $ preEscapedText $ showText y +++ "年" +++ showText m +++ "月のスケジュール"
    $(widgetFile "schedule")
  where
    ywd2cell c (y,w,d) = let d' = fromWeekDate y w d in (d', classOf d' d c)
    fday = fromGregorian y m 1
    lday = fromGregorian y m $ gregorianMonthLength y m
    (fy, fm, _) = toWeekDate fday
    (ly, lm, _) = toWeekDate lday
    classOf :: Day -> Int -> Day -> String
    classOf day d today = intercalate " " 
                          $ ["schedule-day-cell", toWeekName d] 
                           ++ (if today == day then ["today"] else [])
                           ++ (if currentMonth day then ["currentMonth"] else ["otherMonth"])
    taskUri :: Day -> Route BISocie
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
  (uid, r) <- (,) <$> requireAuthId <*> getUrlRender
  issues <- runDB $ do
    ptcpts <- selectList [ParticipantsUser ==. uid] []
    selectList
      [ IssueLimitdate ==. Just day
      , IssueProject <-. map (participantsProject.entityVal) ptcpts
      ] 
      [Asc IssueLimittime]
  jsonToRepJson $ object ["tasks" .= array (map (go r) issues)]
  where
    day = fromGregorian y m d
    go r (Entity iid issue) = 
      object [ "id" .= show iid
             , "subject" .= issueSubject issue
             , "uri" .= r (IssueR (issueProject issue) (issueNumber issue))
             , "limittime" .= showLimittime issue
             ]

postExportCsvR :: UserId -> Handler (RepCsv Text)
postExportCsvR uid = do
  today <- liftIO $ fmap utctDay getCurrentTime
  issues <- runDB $ do
    ptcpts <- selectList [ParticipantsUser ==. uid] []
    selectList
      [ IssueLimitdate >=. Just today
      , IssueProject <-. map (participantsProject.entityVal) ptcpts
      ]
      [Asc IssueLimitdate, Asc IssueLimittime]
  download "task-schedule.csv" (CSV $ generateCsv issues)
  where
    generateCsv :: [Entity Issue] -> ([Text], [[Text]])
    generateCsv iss = (header, map (mkRecord.entityVal) iss)
    header :: [Text]
    header = ["Subject","Start Date","Start Time","End Date","End Time"]
    mkRecord :: Issue -> [Text]
    mkRecord is = sbj:sd:st:ed:et:[]
      where
        sbj = escape $ issueSubject is
        sd = toText $ fromJust $ issueLimitdate is
        st = toText $ fromJust $ issueLimittime is
        (ed, et) = (sd, st)
        escape t =  "\"" <> T.foldr ((<>).rep) T.empty t <> "\""
        rep :: Char -> Text
        rep '\\' = "\\\\"
        rep ',' = "\\,"
        rep '"' = "\\\""
        rep x = T.singleton x

getProjectListR :: Handler RepJson
getProjectListR = do
  (u, r) <- (,) <$> requireAuth <*> getUrlRender
  (includeTerminated, project_name, user_ident_or_name, mpage, ordName) <-
    (,,,,) <$> fmap isJust (lookupGetParam "includeterminated")
    <*> lookupGetParam "project_name"
    <*> lookupGetParam "user_ident_or_name"
    <*> fmap (fmap readText) (lookupGetParam "page")
    <*> fmap (fromMaybe "DescProjectUdate") (lookupGetParam "order")
  let tf = if includeTerminated then [] else [ProjectTerminated ==. False]
  let order = [textToOrder ordName]
  prjs' <- runDB $ do
    pats <- case user_ident_or_name of
      Nothing -> selectList [] []
      Just q -> do
        users <- selectList [] []
        let uids = map entityKey $ filter (userIdentOrName q.entityVal) users
        selectList [ParticipantsUser <-. uids] []
    let pf = [ProjectId <-. map (participantsProject.entityVal) pats]
    if isAdmin (entityVal u)
      then selectList (tf++pf) order
      else do
      ps <- selectList [ParticipantsUser ==. entityKey u] []
      selectList (tf ++ pf ++ [ProjectId <-. (map (participantsProject.entityVal) ps)]) order
  let allprjs = case project_name of
        Just pn -> filter (T.isInfixOf pn . projectName . entityVal) prjs'
        Nothing -> prjs'
      pageLength = ceiling (fromIntegral (length allprjs) / fromIntegral projectListLimit)
      prjs = case mpage of
        Nothing -> allprjs
        Just n  -> drop (n*projectListLimit) $ take ((n+1)*projectListLimit) allprjs
  jsonToRepJson $ object [ "projects" .= array (map (go r) prjs)
                         , "page" .= maybe 0 id mpage
                         , "order" .= ordName
                         , "pageLength" .= (pageLength :: Int)
                         ]
  where
    go r (Entity pid p) = object [ "pid" .= show pid
                                 , "name" .= projectName p
                                 , "description" .= unTextarea (projectDescription p)
                                 , "cdate" .= showDate (projectCdate p)
                                 , "udate" .= showDate (projectUdate p)
                                 , "issuelistUri" .= r (IssueListR pid)
                                 , "projectUri" .= r (ProjectR pid)
                                 ]

getAssignListR :: Handler RepJson
getAssignListR = do
  u <- requireAuth
  pids <- fmap (fmap readText) $ lookupGetParams "projectid"
  users <- runDB $ do
    ps <- if isAdmin (entityVal u)
          then selectList [ParticipantsProject <-. pids] []
          else do
            ps' <- selectList [ParticipantsUser ==. entityKey u, ParticipantsProject <-. pids] []
            selectList [ParticipantsProject <-. (map (participantsProject.entityVal) ps')] []
    selectList [UserId <-. (map (participantsUser.entityVal) ps)] []
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ object ["assigns" .= array (map go users)]
  where
    go (Entity uid u) = object [ "uid" .= show uid
                               , "name" .= userFullName u
                               ]

getStatusListR :: Handler RepJson
getStatusListR = do
  u <- requireAuth
  pids <- fmap (fmap readText) $ lookupGetParams "projectid"
  stss <- runDB $ do
    prjs <- if isAdmin (entityVal u)
            then selectList [ProjectId <-. pids] []
            else do
              ps <- selectList [ParticipantsUser ==. entityKey u, ParticipantsProject <-. pids] []
              selectList [ProjectId <-. (map (participantsProject.entityVal) ps)] []
    return $ nub $ concatMap (\(Entity _ prj) -> 
                               let (Right es) = parseStatuses $ projectStatuses prj 
                               in map fst3 es) prjs
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ object ["statuses" .= array stss]

getCrossSearchR :: Handler RepHtml
getCrossSearchR = do
  u <- requireAuth
  prjs <- runDB $ do
    -- 初回GETなので終了プロジェクトは除外.
    prjs' <- if isAdmin (entityVal u)
             then selectList [ProjectTerminated ==. False] []
             else do
               ps <- selectList [ParticipantsUser ==. entityKey u] []
               selectList [ ProjectTerminated ==. False
                          , ProjectId <-. (map (participantsProject.entityVal) ps)] []
    return $ map toProjectBis prjs'
  defaultLayout $ do
    setTitle "クロスサーチ"
    $(widgetFile "crosssearch")

postCrossSearchR :: Handler RepJson
postCrossSearchR = do
  (u, r) <- (,) <$> requireAuth <*> getUrlRender
  (ps, ss, as) <- 
    (,,) <$> fmap (fmap readText) (lookupPostParams "projectid")
    <*> lookupPostParams "status"
    <*> fmap (fmap (Just . readText)) (lookupPostParams "assign")
  (lf, lt) <- uncurry (liftM2 (,))
              (fmap (fmap (Just . readText)) $ lookupPostParam "limitdatefrom",
               fmap (fmap (Just . addDays 1 . readText)) $ lookupPostParam "limitdateto")
  (uf, ut) <- uncurry (liftM2 (,))
              (fmap (fmap (localDayToUTC . readText)) $ lookupPostParam "updatedfrom",
               fmap (fmap (localDayToUTC . addDays 1 . readText)) $ lookupPostParam "updatedto")
  page <- fmap (max 0 . fromMaybe 0 . fmap readText) $ lookupPostParam "page"
  issues <- runDB $ do
    prjs <- if isAdmin (entityVal u)
            then selectList [ProjectId <-. ps] []
            else do
              ps' <- selectList [ParticipantsUser ==. entityKey u, ParticipantsProject <-. ps] []
              selectList [ProjectId <-. (map (participantsProject.entityVal) ps')] []
    let (pS, sS, aS) = (toInFilter (IssueProject <-.) $ map entityKey prjs,
                        toInFilter (IssueStatus <-.) ss, 
                        toInFilter (IssueAssign <-.) as)
        (lF, lT, uF, uT) = (maybeToFilter (IssueLimitdate >=.) lf,
                            maybeToFilter (IssueLimitdate <.) lt,
                            maybeToFilter (IssueUdate >=.) uf,
                            maybeToFilter (IssueUdate <.) ut)
    issues' <- selectList (pS ++ sS ++ aS ++ lF ++ lT ++ uF ++ uT) [Desc IssueUdate, LimitTo issueListLimit, OffsetBy (page*issueListLimit)]
    forM issues' $ \(Entity id' i) -> do
      cu <- get404 $ issueCuser i
      uu <- get404 $ issueUuser i
      mau <- case issueAssign i of
        Nothing -> return Nothing
        Just auid -> get auid
      let (Just prj) = lookupProjectBis (issueProject i) $ map toProjectBis prjs
      return $ (prj, IssueBis id' i cu uu mau)
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ object ["issues" .= array (map (go r) issues)]
  where
    colorAndEffect s es = case lookupStatus s es of
      Nothing -> ("", "")
      Just (_, c, e) -> (fromMaybe "" c, fromMaybe "" (fmap show e))
    go r (p, i) = 
      let (c, e) = colorAndEffect (issueStatus $ issueBisIssue i) (projectBisStatuses p)
          projectRoute = IssueListR $ projectBisId p
          issueRoute = IssueR (projectBisId p) (issueNumber $ issueBisIssue i)
      in
      object [ "id" .= show (issueBisId i)
             , "effect" .= e
             , "color" .= c
             , "project" .= projectBisName p
             , "projecturi" .= r (projectRoute)
             , "no" .= issueNumber (issueBisIssue i)
             , "subject" .= issueSubject (issueBisIssue i)
             , "issueuri" .= r issueRoute
             , "status" .= issueStatus (issueBisIssue i)
             , "assign" .= showmaybe (fmap userFullName (issueBisAssign i))
             , "limitdate" .= showLimitdate (issueBisIssue i)
             , "limittime" .= showLimittime (issueBisIssue i)
             , "creator" .= userFullName (issueBisCreator i)
             , "updator" .= userFullName (issueBisUpdator i)
             , "updated" .= showDate (issueUdate (issueBisIssue i))
             ]
                
getIssueListR :: ProjectId -> Handler RepHtml
getIssueListR pid = do
  page' <- lookupGetParam "page"
  let page = max 0 $ fromMaybe 0  $ fmap readText $ page'
  (alliis, issues'', prj, es) <- runDB $ do
    prj' <- get404 pid
    let (Right es) = parseStatuses $ projectStatuses prj'
        prj = ProjectBis { projectBisId=pid
                         , projectBisName=projectName prj'
                         , projectBisDescription=projectDescription prj'
                         , projectBisStatuses=es
                         }
    issues <- selectList [IssueProject ==. pid] [Desc IssueUdate]
    let issues' = take issueListLimit $ drop (page*issueListLimit) issues
    issues'' <- forM issues' $ \(Entity id' i) -> do
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
    setTitle $ preEscapedText $ projectBisName prj +++ "タスク一覧"
    $(widgetFile "issuelist")

getNewIssueR :: ProjectId -> Handler RepHtml
getNewIssueR pid = do
  mparent <- lookupGetParam "parent"
  (ptcpts, stss, prj) <- runDB $ do
    prj <- get404 pid
    ptcpts <- selectParticipants pid
    let (Right stss) = parseStatuses $ projectStatuses prj
    return (ptcpts, stss, prj)
  defaultLayout $ do
    setTitle "新規タスク作成"
    $(widgetFile "newissue")
      
postNewIssueR :: ProjectId -> Handler RepHtml
postNewIssueR pid = do
  (uid, r, now) <- 
    (,,) <$> requireAuthId <*> getUrlRender <*> liftIO getCurrentTime
  issue <- runInputPost $ Issue pid undefined uid now uid now
           <$> ireq textField "subject"
           <*> fmap (fmap readText) (iopt textField "assign")
           <*> ireq textField "status"
           <*> iopt dayField "limitdate"
           <*> iopt timeField "limittime"
           <*> iopt dayField "reminderdate"
           <*> fmap (fmap readText) (iopt hiddenField "parent")
  comment <- runInputPost $ Comment pid undefined undefined undefined uid now
             <$> iopt textareaField "content"
             <*> fmap (fmap readText) (iopt textField "assign")
             <*> ireq textField "status"
             <*> iopt dayField "limitdate"
             <*> iopt timeField "limittime"
             <*> iopt dayField "reminderdate"
             <*> ireq boolField "checkreader"
  mfi <- lookupFile "attached"
  ino <- runDB $ do
    update pid [ProjectIssuecounter +=. 1, ProjectUdate =. now]
    prj <- get404 pid
    let ino = projectIssuecounter prj
    mfh <- storeAttachedFile uid mfi
    iid <- insert $ issue {issueNumber=ino}
    cid <- insert $ comment { commentIssue=iid
                            , commentAttached=fmap fst mfh
                            , commentAutomemo=Textarea "init."
                            }
    bcc <- selectMailAddresses pid
    let msgid = toMessageId iid cid now mailMessageIdDomain
        fragment = "#" +++ toPathPiece cid
        url = r (IssueR pid ino) <> fragment
        mfurl = fmap (r . AttachedFileR cid . fst) mfh
    when (isJust (commentContent comment) && not (null bcc)) $
      liftIO $ renderSendMail Mail
            { mailFrom = fromEmailAddress
            , mailTo = []
            , mailCc = []
            , mailBcc = bcc
            , mailHeaders =
              [ ("Subject", issueSubject issue)
              , ("Message-ID", msgid)
              , (mailXHeader, toPathPiece pid)
              ]
            , mailParts = 
                [[ Part "text/plain; charset=utf-8" QuotedPrintableText Nothing []
                   $ LE.encodeUtf8 $ mkTextPart prj issue comment url mfurl
                 , Part "text/html; charset=utf-8" QuotedPrintableText Nothing []
                   $ LE.encodeUtf8 $ mkHtmlPart prj issue comment url mfurl
                 ]]
            }
    return ino
  redirect $ IssueR pid ino


-- | FIXME: like a mkMail on Root.hs
mkTextPart p i c url mfUrl = [stext|
プロジェクト: #{projectName p}
タスク: #{issueSubject i}
ステータス: #{issueStatus i}

 #{unTextarea $ fromJust $ commentContent c}

* このメールに直接返信せずにこちらのページから投稿してください.
URL: #{url}
|] <> if isNothing mfUrl then "" else [stext|
添付ファイル: #{fromJust mfUrl}
|]

mkHtmlPart p i c url mfUrl = LE.decodeUtf8 $ renderHtml [shamlet|
<p>
  <dl>
    <dt>プロジェクト
    <dd>#{projectName p}
    <dt>タスク
    <dd>#{issueSubject i}
    <dt>ステータス
    <dd>#{issueStatus i}

<p>#{unTextarea $ fromJust $ commentContent c}

<p> * このメールに直接返信せずにこちらのページから投稿してください.
<p>
  <dl>
    <dt>URL
    <dd>#{url}
    $maybe furl <- mfUrl
      <dt>添付ファイル
      <dd>#{furl}
|]

getIssueR :: ProjectId -> IssueNo -> Handler RepHtml
getIssueR pid ino = do
  selfid <- requireAuthId
  (prj, ptcpts, iid, issue, comments, mparent, children) <- 
    runDB $ do
      (Entity iid issue) <- getBy404 $ UniqueIssue pid ino
      cs <- selectList [CommentIssue ==. iid] [Desc CommentCdate]
      comments <- forM cs $ \(Entity cid c) -> do
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
      mparent <- getMaybe $ issueParentIssue issue
      children <- selectList [IssueParentIssue ==. Just iid] []
      return (prj, ptcpts, iid, issue, comments, mparent, children)
  let (Right stss) = parseStatuses $ projectStatuses prj
      isAssign = case issueAssign issue of
        Nothing -> const False
        Just uid -> (==uid)
      isStatus = (==issueStatus issue)
  defaultLayout $ do
    setTitle $ preEscapedText $ issueSubject issue
    $(widgetFile "issue")

getMaybe :: (PersistStore m, PersistEntity a,
             PersistMonadBackend m ~ PersistEntityBackend a) =>
            Maybe (Key a) -> m (Maybe a)
getMaybe Nothing = return Nothing
getMaybe (Just k) = get k

postCommentR :: ProjectId -> IssueNo -> Handler RepHtml
postCommentR pid ino = do
  uid <- requireAuthId
  now <- liftIO getCurrentTime
  comment <- runInputPost $ Comment pid undefined undefined undefined uid now
             <$> iopt textareaField "content"
             <*> fmap (fmap readText) (iopt textField "assign")
             <*> ireq textField "status"
             <*> iopt dayField "limitdate"
             <*> iopt timeField "limittime"
             <*> iopt dayField "reminderdate"
             <*> ireq boolField "checkreader"
  mfi <- lookupFile "attached"
  runDB $ do
    r <- lift getUrlRender
    (Entity iid issue) <- getBy404 $ UniqueIssue pid ino
    Just (Entity lastCid lastC) <- selectFirst [CommentIssue ==. iid] [Desc CommentCdate]
    mfh <- storeAttachedFile uid mfi
    amemo <- generateAutomemo comment issue mfh
    replace iid issue { issueUuser = uid
                      , issueUdate = now
                      , issueLimitdate = commentLimitdate comment
                      , issueLimittime = commentLimittime comment
                      , issueReminderdate = commentReminderdate comment 
                      , issueAssign = commentAssign comment
                      , issueStatus = commentStatus comment
                      }
    when (isNothing (commentContent comment) && T.null (unTextarea amemo)) $ do
      lift $ do
        r <- getMessageRender
        setPNotify $ PNotify JqueryUI Error "invalid input" $ r MsgInvalidCommentPosted
        redirect $ IssueR pid ino
    cid <- insert $ comment { commentIssue=iid
                            , commentAttached=fmap fst mfh
                            , commentAutomemo=amemo
                            }
    prj <- get404 pid
    emails <- selectMailAddresses pid
    let msgid = toMessageId iid cid now mailMessageIdDomain
        refid = toMessageId iid lastCid (commentCdate lastC) mailMessageIdDomain
        fragment = "#" +++ toPathPiece cid
        url = r (IssueR pid ino) <> fragment
        mfurl = fmap (r . AttachedFileR cid . fst) mfh
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
          , (mailXHeader, toPathPiece pid)
          ]
        , mailParts = 
            [[ Part "text/plain; charset=utf-8" QuotedPrintableText Nothing []
               $ LE.encodeUtf8 $ mkTextPart prj issue comment url mfurl
             ]]
        }
  redirect $ IssueR pid ino
        
getAttachedFileR :: CommentId -> FileHeaderId -> Handler RepHtml
getAttachedFileR cid fid = do
  f <- runDB $ get404 fid
  getFileR (fileHeaderCreator f) fid
  
postReadCommentR :: CommentId -> Handler RepJson
postReadCommentR cid = do
  (Entity uid u) <- requireAuth
  r <- getUrlRender
  ret <- runDB $ do
    cmt <- get404 cid
    mr <- getBy $ UniqueReader cid uid
    case mr of
      Just _ -> return "added"
      Nothing -> do
        now <- liftIO getCurrentTime
        _ <- insert $ Reader cid uid now
        return "added"
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ object [ "status" .= (ret :: Text)
                         , "read" .=
                           object [ "comment" .= show cid
                                  , "reader" .=
                                    object [ "id" .= show uid
                                           , "ident" .= userIdent u
                                           , "name" .= userFullName u
                                           , "uri" .= r (ProfileR uid)
                                           , "avatar" .= r (AvatarImageR uid)
                                           ]
                                  ]
                         ]

deleteReadCommentR :: CommentId -> Handler RepJson
deleteReadCommentR cid = do
  (Entity uid u) <- requireAuth
  r <- getUrlRender
  ret <- runDB $ do
    cmt <- get404 cid
    deleteBy $ UniqueReader cid uid
    return "deleted"
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ object [ "status" .= (ret :: Text)
                         , "read" .=
                           object [ "comment" .= show cid
                                  , "reader" .=
                                    object [ "id" .= show uid
                                           , "ident" .= userIdent u
                                           , "name" .= userFullName u
                                           , "uri" .= r (ProfileR uid)
                                           , "avatar" .= r (AvatarImageR uid)
                                           ]
                                  ]
                         ]
  

getCommentReadersR :: CommentId -> Handler RepJson
getCommentReadersR cid = do
  r <- getUrlRender
  readers <- runDB $ do
    cmt <- get404 cid
    rds' <- selectList [ReaderComment ==. cid] [Asc ReaderCheckdate]
    forM rds' $ \(Entity _ rd') -> do
      let uid' = readerReader rd'
          ra = AvatarImageR uid'
      Just u <- get uid'
      return (uid', u, ra)
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ object ["readers" .= array (map (go r) readers)]
  where
    go r (uid, u, ra) = object [ "id" .= show uid
                               , "ident" .= userIdent u
                               , "name" .= userFullName u
                               , "uri" .= r (ProfileR uid)
                               , "avatar" .= r ra
                               ]

-- selectParticipants :: (Failure ErrorResponse m, MonadTrans t, PersistBackend t m) =>
--     Key t (ProjectGeneric t) -> t m [(Key t User, User)]
selectParticipants pid = do
  mapM (p2u.entityVal) =<< selectList [ParticipantsProject ==. pid] []
  where
    p2u p = do
      let uid = participantsUser p
      u <- get404 uid
      return (uid, u)

storeAttachedFile _ Nothing = return Nothing
storeAttachedFile uid (Just fi) = fmap (fmap fst5'snd5) $ upload uid fi
  where
    fst5'snd5 (x,y,_,_,_) = (x,y)

generateAutomemo c i f = do
  let st = if issueStatus i == commentStatus c
           then []
           else [[stext|ステータス #{issueStatus i} から #{commentStatus c} に変更.|]]
      lm = case (issueLimitDatetime i, commentLimitDatetime c) of
        (Nothing, Nothing) -> []
        (Just x , Nothing) -> [[stext|期限 #{showDate x} を期限なしに変更.|]]
        (Nothing, Just y ) -> [[stext|期限を #{showDate y} に設定.|]]
        (Just x , Just y ) -> if x == y
                              then []
                              else [[stext|期限を #{showDate x} から #{showDate y} に変更.|]]
      rm = case (issueReminderdate i, commentReminderdate c) of
        (Nothing, Nothing) -> []
        (Just x , Nothing) -> [[stext|通知日 #{showText x} を通知なしに変更.|]]
        (Nothing, Just y ) -> [[stext|通知日を #{showText y} に設定.|]]
        (Just x , Just y ) -> if x == y
                              then []
                              else [[stext|通知日を #{showText x} から #{showText y} に変更.|]]

      af = case f of
        Nothing -> []
        Just (_, fname) -> [[stext|ファイル #{fname} を添付.|]]
  as <- case (issueAssign i, commentAssign c) of
    (Nothing, Nothing) -> return []
    (Just x , Nothing) -> do
      x' <- get404 x
      return [[stext|担当者 #{userFullName x'} を担当者なしに変更.|]]
    (Nothing, Just y ) -> do
      y' <- get404 y
      return [[stext|担当者を #{userFullName y'} に設定.|]]
    (Just x , Just y ) -> do
      x' <- get404 x
      y' <- get404 y
      if x' == y'
        then return []
        else return [[stext|担当者を #{userFullName x'} から #{userFullName y'} に変更.|]]
  return $ Textarea $ fromLazy $ L.intercalate "\n" $ st <> as <> lm <> rm <> af
  where
    fromLazy = T.pack . L.unpack
