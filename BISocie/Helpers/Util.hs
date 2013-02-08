module BISocie.Helpers.Util 
       ( mkPagenate
       , (+++)
       , showText
       , readText
       , encodeUrl
       , decodeUrl
         -- RSA
       , genKey
       , encrypt
       , decrypt
       , sign
       , verify
       ) where

import Codec.Binary.UTF8.String (encodeString, decodeString)
import qualified Codec.Crypto.RSA as RSA
import Control.Arrow (first)
import Crypto.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as T
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

-- |
--
-- RSA utility
--
fromLazy :: BL.ByteString -> BS.ByteString
fromLazy = BS.pack . BL.unpack
toLazy :: BS.ByteString -> BL.ByteString
toLazy = BL.pack . BS.unpack

genKey :: IO (RSA.PublicKey, RSA.PrivateKey)
genKey = (newGenIO::IO SystemRandom) >>= return . fs . flip RSA.generateKeyPair 1024
  where
    fs (f, s, _) = (f, s)

encode :: BL.ByteString -> BL.ByteString
encode = toLazy . Base64.encode . fromLazy
decode :: BL.ByteString -> BL.ByteString
decode = either BL.pack toLazy . Base64.decode . fromLazy

encrypt :: RSA.PublicKey -> BL.ByteString -> IO (BL.ByteString, SystemRandom)
encrypt pub plain = do
  g <- newGenIO :: IO SystemRandom
  return $ first encode $ RSA.encrypt g pub plain

decrypt :: RSA.PrivateKey -> BS.ByteString -> BS.ByteString
decrypt priv cipher = either BS.pack (fromLazy . RSA.decrypt priv . toLazy) $ Base64.decode cipher

sign :: RSA.PrivateKey -> BL.ByteString -> BL.ByteString
sign = (encode.).RSA.sign

verify :: RSA.PublicKey -> BL.ByteString -> BL.ByteString -> Bool
verify pub plain cipher = RSA.verify pub plain $ decode cipher
