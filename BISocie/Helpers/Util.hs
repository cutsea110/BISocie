module BISocie.Helpers.Util 
       ( mkPagenate
       , (+++)
       , showText
       , readText
       , encodeUrl
       , decodeUrl
       ) where

import Data.Text (Text)
import qualified Data.Text as T
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Network.HTTP.Base (urlEncode, urlDecode)

(+++) :: Text -> Text -> Text
(+++) = T.append
showText :: (Show a) => a -> Text
showText = T.pack . show
readText :: (Read a) => Text -> a
readText = read . T.unpack

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
