{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
module Handler.Project where

import BISocie
import Control.Monad (unless, forM, mplus)
import Control.Applicative ((<$>),(<*>))
import Data.Time

import qualified Settings
import StaticFiles

getNewProjectR :: Handler RepHtml
getNewProjectR = do
  (selfid, self) <- requireAuth
  let cancreateproject = userRole self >= Teacher
      viewablehumannet = userRole self >= Teacher
  unless cancreateproject $ 
    permissionDenied "あなたはプロジェクトを作成することはできません."
  let inintstatuses = "!未開始#赤\n着手#緑\n完了#灰\n=却下#灰\n保留\n議論\n報告"::String
      help = $(Settings.hamletFile "help")
  defaultLayout $ do
    setTitle $ string "プロジェクト新規作成"
    addCassius $(cassiusFile "project")
    addJulius $(juliusFile "help")
    addHamlet $(hamletFile "newproject")
    
postNewProjectR :: Handler RepHtml
postNewProjectR = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "create" -> createProject
    _             -> invalidArgs ["The possible values of '_method' is create"]
  where
    createProject :: Handler RepHtml
    createProject = do
      (selfid, self) <- requireAuth
      let cancreateproject = userRole self >= Teacher
      unless cancreateproject $ 
        permissionDenied "あなたはプロジェクトを作成することはできません."
      (name, desc, sts) <- runFormPost'$ (,,)
                           <$> stringInput "name"
                           <*> stringInput "description"
                           <*> stringInput "statuses"
      now <- liftIO getCurrentTime
      runDB $ do
        pid <- insert $ Project { projectName=name
                                , projectIssuecounter=0
                                , projectDescription=desc
                                , projectStatuses=sts
                                , projectCuser=selfid
                                , projectCdate=now
                                , projectUdate=now
                                }
        _ <- insert $ Participants { participantsProject=pid 
                                   , participantsUser=selfid 
                                   , participantsReceivemail=True
                                   }
        lift $ redirect RedirectTemporary $ ProjectR pid

getProjectR :: ProjectId -> Handler RepHtml
getProjectR pid = do
  (selfid, self) <- requireAuth
  runDB $ do 
    p <- getBy $ UniqueParticipants pid selfid
    let viewable = p /= Nothing
        editable = viewable && userRole self >= Teacher
    unless viewable $ 
      lift $ permissionDenied "あなたはこのプロジェクトの参加者ではありません."
    prj <- get404 pid
    now <- liftIO getCurrentTime
    let (y,_,_) = toGregorian $ utctDay now
        eyears = [Settings.entryStartYear..y+5]
        help = $(Settings.hamletFile "help")
    lift $ defaultLayout $ do
      setTitle $ string $ projectName prj
      addCassius $(cassiusFile "project")
      addJulius $(juliusFile "help")
      addJulius $(juliusFile "project")
      addHamlet $(hamletFile "project")


postProjectR :: ProjectId -> Handler RepJson
postProjectR pid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "modify" -> putProjectR pid
    _             -> invalidArgs ["The possible values of '_method' is modify"]

putProjectR :: ProjectId -> Handler RepJson
putProjectR pid = do
  (selfid, self) <- requireAuth
  runDB $ do
    p <- getBy $ UniqueParticipants pid selfid
    let viewable = p /= Nothing
        editable = viewable && userRole self >= Teacher
    unless editable $ 
      lift $ permissionDenied "あなたはこのプロジェクトの設定を編集できません."
    prj <- get404 pid
    Just nm <- (lift $ lookupPostParam "name") >>=
               \nm' -> return $ nm' `mplus` (Just $ projectName prj)
    Just ds <- (lift $ lookupPostParam "description") >>=
          \ds' -> return $ ds' `mplus` (Just $ projectDescription prj)
    Just st <- (lift $ lookupPostParam "statuses") >>=
               \st' -> return $ st' `mplus` (Just $ projectStatuses prj)
    now <- liftIO getCurrentTime
    update pid [ ProjectName nm
               , ProjectDescription ds
               , ProjectStatuses st
               , ProjectUdate now]
    prj <- get404 pid
    lift $ do
      cacheSeconds 10 -- FIXME
      jsonToRepJson $ jsonMap [ ("name", jsonScalar $ projectName prj)
                              , ("description", jsonScalar $ projectDescription prj)
                              , ("statuses", jsonScalar $ projectStatuses prj)
                              ]
  where
    showJScalar :: (Show a) => a -> Json
    showJScalar = jsonScalar . show
    showMaybeJScalar :: Maybe String -> Json
    showMaybeJScalar = jsonScalar . showmaybe
