{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, CPP #-}
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
import Data.Time
import Data.Int
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Char8 (pack)
import System.Directory
import System.FilePath
import Web.Encodings (encodeUrl)
import Text.Cassius (cassiusFile)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Settings (s3dir)
import BISocie.Helpers.Util ((+++))

getUploadR :: Handler RepHtml
getUploadR = do
  (uid,_) <- requireAuth
  defaultLayout $ do
    addCassius $(cassiusFile "templates/s3/s3.cassius")
    addWidget $(widgetFile "s3/upload")

upload :: PersistBackend b m =>
          Key backend User -> FileInfo -> b m (Maybe (Key b (FileHeaderGeneric backend), Text, Text, Int64, UTCTime))
upload uid fi = do
  if fileName fi /= "" && L.length (fileContent fi) > 0
    then do
    now <- liftIO getCurrentTime
    let (name, ext) = splitExtension $ T.unpack $ fileName fi
        efname = encodeUrl $ fileName fi
        fsize = L.length $ fileContent fi
    fid <-
      insert FileHeader { fileHeaderFullname=fileName fi
                        , fileHeaderEfname=efname
                        , fileHeaderContentType=fileContentType fi
                        , fileHeaderFileSize=fsize
                        , fileHeaderName=T.pack name
                        , fileHeaderExtension=T.pack ext
                        , fileHeaderCreator=uid
                        , fileHeaderCreated=now
                        }
    let s3dir = Settings.s3dir </> show uid
        s3fp = s3dir </> show fid
    liftIO $ do
      createDirectoryIfMissing True s3dir
      L.writeFile s3fp (fileContent fi)
    return $ Just (fid, fileName fi, T.pack ext, fsize, now)
    else return Nothing
  
postUploadR :: Handler RepXml
postUploadR = do
  (uid, _) <- requireAuth
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
                      [xhamlet|\
<file>
  <fhid>#{show fid}
  <name>#{name}
  <ext>#{ext}
  <size>#{show fsize}
  <cdate>#{show cdate}
  <uri>#{rf}
|]

putUploadR :: Handler RepHtml
putUploadR = do
  (uid, _) <- requireAuth
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
  let s3dir = Settings.s3dir </> show uid
      s3fp = s3dir </> show fid
  setHeader "Content-Type" $ pack $ T.unpack $ fileHeaderContentType h
  setHeader "Content-Disposition" $ pack $ T.unpack $ "attachment; filename=" +++ fileHeaderEfname h
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
  (uid'', _) <- requireAuth
  if uid/=uid''
    then
    invalidArgs ["You couldn't delete this resource."]
    else do
    r <- getUrlRender
    runDB $ delete fid
    let s3dir = Settings.s3dir </> show uid
        s3fp = s3dir </> show fid
        rf = r $ FileR uid fid
    liftIO $ removeFile s3fp
    fmap RepXml $ hamletToContent
                  [xhamlet|\
<deleted>
  <uri>#{rf}
|]

getFileListR :: UserId -> Handler RepJson
getFileListR uid = do
  _ <- requireAuth
  render <- getUrlRender
  files <- runDB $ selectList [FileHeaderCreator ==. uid] [Desc FileHeaderCreated]
  cacheSeconds 10 -- FIXME
  jsonToRepJson $ jsonMap [("files", jsonList $ map (go render) files)]
  where
    go r (fid, FileHeader
               { fileHeaderFullname = name
               , fileHeaderExtension = ext
               , fileHeaderFileSize = size
               , fileHeaderCreated = cdate
               }) = 
      jsonMap [ ("name", jsonScalar $ T.unpack name)
              , ("ext" , jsonScalar $ T.unpack ext)
              , ("size", jsonScalar $ show size)
              , ("cdate", jsonScalar $ show cdate)
              , ("uri", jsonScalar $ T.unpack $ r $ FileR uid fid)
              ]
