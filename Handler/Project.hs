{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
module Handler.Project where

import BISocie
import Control.Monad (when, forM, mplus)
import Data.Time

import StaticFiles

getNewProjectR :: Handler RepHtml
getNewProjectR = do
  (uid, u) <- requireAuth
  when (not $ canCreateProject u) $ permissionDenied "あなたはプロジェクトを作成することはできません."
  defaultLayout $ do
    setTitle $ string "プロジェクト新規作成"
    addCassius $(cassiusFile "project")
    addHamlet $(hamletFile "newproject")
    
postNewProjectR :: Handler RepHtml
postNewProjectR = do
  (uid, u) <- requireAuth
  when (not $ canCreateProject u) $ permissionDenied "あなたはプロジェクトを作成することはできません."
  now <- liftIO getCurrentTime
  pid <- runDB $ do
    pid <- insert $ initProject uid now
    _ <- insert $ Participants pid uid True
    return pid
  redirect RedirectTemporary $ ProjectR pid

getProjectR :: ProjectId -> Handler RepHtml
getProjectR pid = do
  (uid, u) <- requireAuth
  (prj, editable) <- runDB $ do 
    viewable <- uid `canView` pid
    editable <- uid `canEdit` pid
    when (not viewable) $ lift $ permissionDenied "あなたはこのプロジェクトの参加者ではありません."
    prj <- get404 pid
    return (prj, editable)
  defaultLayout $ do
    setTitle $ string $ projectName prj
    addCassius $(cassiusFile "project")
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
  (uid, u) <- requireAuth
  prj <- runDB $ do
     editable <- uid `canEdit` pid
     when (not editable) $ lift $ permissionDenied "あなたはこのプロジェクトの設定を編集できません."
     p <- get404 pid
     Just nm <- (lift $ lookupPostParam "name") >>=
           \nm' -> return $ nm' `mplus` (Just $ projectName p)
     ds <- (lift $ lookupPostParam "description") >>=
           \ds' -> return $ ds' `mplus` projectDescription p
     Just st <- (lift $ lookupPostParam "statuses") >>=
           \st' -> return $ st' `mplus` (Just $ projectStatuses p)
     now <- liftIO getCurrentTime
     update pid [ProjectName nm, ProjectDescription ds, ProjectStatuses st, ProjectUdate now]
     get404 pid
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [ ("name", jsonScalar $ projectName prj)
                          , ("description", showMaybeJScalar $ projectDescription prj)
                          , ("statuses", jsonScalar $ projectStatuses prj)
                          ]
    where
      showJScalar :: (Show a) => a -> Json
      showJScalar = jsonScalar . show
      showMaybeJScalar :: Maybe String -> Json
      showMaybeJScalar = jsonScalar . showmaybe
