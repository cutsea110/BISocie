{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
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
    addCassius $(cassiusFile "project")
    addJulius $(juliusFile "project")
    addHamlet $(hamletFile "project")


postProjectR :: ProjectId -> Handler RepXml
postProjectR pid = do
  _method <- lookupPostParam "_method"
  case _method of
    Just "modify" -> putProjectR pid
    _             -> invalidArgs ["The possible values of '_method' is modify"]

putProjectR :: ProjectId -> Handler RepXml
putProjectR pid = do
  (uid, u) <- requireAuth
  nm' <- lookupPostParam "name"
  ds' <- lookupPostParam "description"
  st' <- lookupPostParam "statuses"
  (nm, ds, st) <- runDB $ do
     mp' <- getBy $ UniqueParticipants pid uid
     case mp' of
       Nothing -> lift $ permissionDenied "あなたはこのプロジェクトの参加者ではありません."
       Just _ -> do 
         p <- get404 pid
         let (Just nm, ds, Just st) = 
               ( nm' `mplus` (Just $ projectName p)
               , ds' `mplus` projectDescription p
               , st' `mplus` (Just $ projectStatuses p)
               )
         update pid [ProjectName nm, ProjectDescription ds, ProjectStatuses st]
         return (nm, ds, st)
  fmap RepXml $ hamletToContent
#if GHC7
                  [xhamlet|
#else
                  [$xhamlet|
#endif
%project
  %name $nm$
  $maybe ds ds
    %description $ds$
  $nothing
    %description
  %statuses $st$
|]
