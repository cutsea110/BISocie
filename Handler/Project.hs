{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Project where

import BISocie
import Control.Monad (when, forM, mplus)

getProjectR :: ProjectId -> Handler RepHtml
getProjectR pid = do
  (uid, u) <- requireAuth
  prj <- runDB $ do 
    mp <- getBy $ UniqueParticipants pid uid
    case mp of
      Nothing -> lift $ permissionDenied "あなたはこのプロジェクトの参加者ではありません."
      Just _ -> get404 pid
  defaultLayout $ do
    setTitle $ string $ projectName prj
    addJulius $(juliusFile "project")
    addHamlet $(hamletFile "project")


postProjectR :: ProjectId -> Handler ()
postProjectR pid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "modify" -> putProjectR pid
    _             ->invalidArgs ["The possible values of '_method' is modify"]

putProjectR :: ProjectId -> Handler ()
putProjectR pid = do
  (uid, u) <- requireAuth
  p <- runDB $ do
     mp' <- getBy $ UniqueParticipants pid uid
     case mp' of
       Nothing -> lift $ permissionDenied "あなたはこのプロジェクトの参加者ではありません."
       Just _ -> get404 pid
  nm' <- lookupPostParam "name"
  ds' <- lookupPostParam "description"
  st' <- lookupPostParam "statuses"
  let (Just nm, ds, Just st) = ( nm' `mplus` (Just $ projectName p)
                               , ds' `mplus` projectDescription p
                               , st' `mplus` (Just $ projectStatuses p)
                               )
  runDB $ update pid [ProjectName nm, ProjectDescription ds, ProjectStatuses st]
  setMessage "プロジェクトを更新しました."
  redirect RedirectTemporary $ ProjectR pid
