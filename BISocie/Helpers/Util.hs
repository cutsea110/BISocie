{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module BISocie.Helpers.Util
       ( mkPagenate
       , (+++)
       , showText
       , readText
       , encodeUrl
       , decodeUrl
       , fst3
       , snd3
       , thd3
       , ilike
         -- for CSV
       , RepCsv(..)
       , CSV(..)
       , ToText(..)
       , download
       ) where

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Control.Arrow ((***))
import Database.Persist
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Tuple.HT (fst3, snd3, thd3)
import GHC.Int (Int64)
import Network.HTTP.Base (urlEncode, urlDecode)
import Yesod

(+++) :: Text -> Text -> Text
(+++) = T.append
showText :: (Show a) => a -> Text
showText = T.pack . show
readText :: (Read a) => Text -> a
readText = read . T.unpack

ilike :: EntityField v Text -> Text -> Filter v
ilike field val = Filter field (Left $ T.concat ["%", escape val, "%"]) (BackendSpecificFilter "ILIKE")
  where
    escape = T.foldr esc ""
    esc c t | T.any (==c) "%?'" = '\\' `T.cons` c `T.cons` t
            | otherwise = c `T.cons` t

mkPagenate :: Int -> Int -> Int -> Int -> [[Int]]
mkPagenate fillGap width current maxpage =
  if leftConnected && rightConnected
  then [[ll..rr]]
  else if leftConnected
       then [[ll..cr], [rl..rr]]
       else if rightConnected
            then [[ll..lr],[cl..rr]]
            else [[ll..lr],[cl..cr],[rl..rr]]
  where
    leftConnected = cl-lr <= fillGap
    rightConnected = rl-cr <= fillGap
    ll = 0
    lr = width
    cl = current-width
    cr = current+width
    rl = maxpage-width
    rr = maxpage

encodeUrl :: T.Text -> T.Text
encodeUrl = T.pack . urlEncode . encodeString . T.unpack

decodeUrl :: T.Text -> T.Text
decodeUrl = T.pack . decodeString . urlDecode . T.unpack


newtype CSV a = CSV { unCsv :: ([Text],[[a]]) } deriving Show

instance ToText a => ToContent (CSV a) where
  toContent = toContent.trans
    where
      trans :: ToText a => CSV a -> Text
      trans = T.unlines.uncurry (:).(xHead *** xBody).unCsv
      xBody :: ToText a => [[a]] -> [Text]
      xBody = map (T.intercalate "," . map toText)
      xHead  :: [Text] -> Text
      xHead = T.intercalate ","

class ToText a where
  toText :: a -> Text

instance ToText String where
  toText = T.pack
instance ToText Text where
  toText = id
instance ToText Int where
  toText = T.pack . show
instance ToText Double where
  toText = T.pack . show
instance ToText Integer where
  toText = T.pack . show
instance ToText Day where
  toText = T.pack . show
instance ToText TimeOfDay where
  toText = T.pack . show
instance ToText UTCTime where
  toText = T.pack . show
instance ToText Int64 where
  toText = T.pack . show

newtype RepCsv a = RepCsv (CSV a)

instance ToText a => ToContent (RepCsv a) where
  toContent (RepCsv csv) = toContent csv

instance (ToText a, ToContent (RepCsv a)) => ToTypedContent (RepCsv a) where
  toTypedContent (RepCsv csv) = TypedContent typeOctet (toContent csv)
instance ToTypedContent (RepCsv a) => HasContentType (RepCsv a) where
  getContentType _ = typeOctet

download :: ToText a => Text -> CSV a -> HandlerT m IO (RepCsv a)
download fn csv = do
  addHeader "Content-Disposition" $ "attachment; filename=" `T.append` fn
  return (RepCsv csv)
