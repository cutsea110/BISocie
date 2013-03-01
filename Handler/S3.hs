{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Handler.S3 
       ( getUploadR
       , postUploadR
       , putUploadR
       , getFileR
       , postFileR
       , deleteFileR
       , getFileListR
       , upload -- Internal API
       ) where

import Foundation

import Yesod
import Control.Applicative ((<$>))
import Data.Conduit (($$))
import Data.Conduit.List (consume)
import Data.Time
import qualified Data.ByteString.Lazy as L
import System.Directory
import System.FilePath
import qualified Data.Text as T

import qualified Settings (s3dir)
import BISocie.Helpers.Util ((+++), encodeUrl)

getUploadR :: Handler RepHtml
getUploadR = do
  (Entity uid _) <- requireAuth
  defaultLayout $ do
    $(widgetFile "s3/upload")

upload uid fi = do
  lbs <- lift $ lift $ fileContent fi
  let fsize = L.length lbs
  if fileName' fi /= "" && L.length lbs > 0
    then do
    now <- liftIO getCurrentTime
    let (name, ext) = splitExtension $ T.unpack $ fileName' fi
        efname = encodeUrl $ fileName' fi
    fid <-
      insert FileHeader { fileHeaderFullname=fileName' fi
                        , fileHeaderEfname=efname
                        , fileHeaderContentType=fileContentType fi
                        , fileHeaderFileSize=fsize
                        , fileHeaderName=T.pack name
                        , fileHeaderExtension=T.pack ext
                        , fileHeaderCreator=uid
                        , fileHeaderCreated=now
                        }
    let s3dir = Settings.s3dir </> T.unpack (toPathPiece uid)
        s3fp = s3dir </> T.unpack (toPathPiece fid)
    liftIO $ do
      createDirectoryIfMissing True s3dir
      L.writeFile s3fp lbs
    return $ Just (fid, fileName' fi, T.pack ext, fsize, now)
    else return Nothing
  where
    fileName' :: FileInfo -> T.Text
    fileName' = last . T.split (\c -> c=='/' || c=='\\') . fileName
    fileContent f = L.fromChunks <$> (fileSource f $$ consume)
  
postUploadR :: Handler RepXml
postUploadR = do
  (Entity uid _) <- requireAuth
  mfi <- lookupFile "upfile"
  case mfi of
    Nothing -> invalidArgs ["upload file is required."]
    Just fi -> do
      r <- getUrlRender
      mf <- runDB $ upload uid fi
      case mf of
        Nothing -> invalidArgs ["upload file is required."]
        Just (fid, name, ext, fsize, cdate) -> do
          cacheSeconds 10 -- FIXME
          let rf = r $ FileR uid fid
          fmap RepXml $ hamletToContent
                      [xhamlet|$newline never
<file>
  <fhid>#{T.unpack $ toPathPiece fid}
  <name>#{name}
  <ext>#{ext}
  <size>#{show fsize}
  <cdate>#{show cdate}
  <uri>#{rf}
|]

putUploadR :: Handler RepHtml
putUploadR = do
  (Entity uid _) <- requireAuth
  mfi <- lookupFile "upfile"
  case mfi of
    Nothing -> invalidArgs ["upload file is required."]
    Just fi -> do
      mf <- runDB $ upload uid fi
      case mf of
        Nothing -> invalidArgs ["upload file is required."]
        Just (fid, _, _, _, _) -> sendResponseCreated $ FileR uid fid


getFileR :: UserId -> FileHeaderId -> Handler RepHtml
getFileR uid fid = do
  h <- runDB $ get404 fid
  let s3dir = Settings.s3dir </> T.unpack (toPathPiece uid)
      s3fp = s3dir </> T.unpack (toPathPiece fid)
  setHeader "Content-Type" $ fileHeaderContentType h
  setHeader "Content-Disposition" $ "attachment; filename=" +++ fileHeaderEfname h
  return $ RepHtml $ ContentFile s3fp Nothing

postFileR :: UserId -> FileHeaderId -> Handler RepXml
postFileR uid fid = do
  _ <- requireAuth
  _method <- lookupPostParam "_method"
  case _method of
    Just "delete" -> deleteFileR uid fid
    _ -> invalidArgs ["The possible values of '_method' are delete."]

deleteFileR :: UserId -> FileHeaderId -> Handler RepXml
deleteFileR uid fid = do
  (Entity uid'' _) <- requireAuth
  if uid/=uid''
    then
    invalidArgs ["You couldn't delete this resource."]
    else do
    r <- getUrlRender
    runDB $ delete fid
    let s3dir = Settings.s3dir </> T.unpack (toPathPiece uid)
        s3fp = s3dir </> T.unpack (toPathPiece fid)
        rf = r $ FileR uid fid
    liftIO $ removeFile s3fp
    fmap RepXml $ hamletToContent
                  [xhamlet|$newline never
<deleted>
  <uri>#{rf}
|]

getFileListR :: UserId -> Handler RepJson
getFileListR uid = do
  _ <- requireAuth
  render <- getUrlRender
  files <- runDB $ selectList [FileHeaderCreator ==. uid] [Desc FileHeaderCreated]
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ object ["files" .= array (map (go render) files)]
  where
    go r (Entity fid FileHeader
               { fileHeaderFullname = name
               , fileHeaderExtension = ext
               , fileHeaderFileSize = size
               , fileHeaderCreated = cdate
               }) = 
      array [ "name" .= name
            , "ext" .= ext
            , "size" .= size
            , "cdate" .= cdate
            , "uri" .= r (FileR uid fid)
            ]
