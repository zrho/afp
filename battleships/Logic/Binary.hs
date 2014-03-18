{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Logic.Binary
-- Stability   :  experimental
-- Portability :  portable
--
-- Import/export for binary serializable data to text URLs.

module Logic.Binary
  ( -- * Import/Export
    impBinary
  , expBinary
    -- * Misc
  , fromStrict
  , toStrict
  ) where

import           Prelude
import           Data.Text (Text)
import qualified Data.Text.Encoding          as TE
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString.Base64.Lazy as B64

--------------------------------------------------------------------------------
-- * Import/Export
--------------------------------------------------------------------------------

-- | Imports a byte string from text.
impBinary :: Text -> Maybe BL.ByteString
impBinary
  = eitherToMaybe
  . B64.decode
  . fromStrict
  . fromBase64Url
  . TE.encodeUtf8

-- | Exports a byte string to text.
expBinary :: BL.ByteString -> Text
expBinary
  = TE.decodeUtf8
  . toBase64Url
  . toStrict
  . B64.encode

--------------------------------------------------------------------------------
-- * URL
--
-- Some characters used in base 64 encoding are problematic when used in URLs.
-- These functions convert from and into a representation in which these
-- characters are replaced with non problematic ones.
--------------------------------------------------------------------------------

toBase64Url :: BS.ByteString -> BS.ByteString
toBase64Url = BS.map convert where
  convert 43 = 45 -- '+' --> '-'
  convert 47 = 95 -- '/' --> '_'
  convert  x =  x

fromBase64Url :: BS.ByteString -> BS.ByteString
fromBase64Url = BS.map convert where
  convert 45 = 43 -- '+' <-- '-'
  convert 95 = 47 -- '/' <-- '_'
  convert  x =  x

--------------------------------------------------------------------------------
-- * Misc
--------------------------------------------------------------------------------

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe e = case e of
  Right x -> Just x
  _       -> Nothing

#if MIN_VERSION_bytestring(0,10,0)
toStrict :: BL.ByteString -> BS.ByteString
toStrict = BL.toStrict

fromStrict :: BS.ByteString -> BL.ByteString
fromStrict = BL.fromStrict
#else
toStrict :: BL.ByteString -> BS.ByteString
toStrict = BS.concat . BL.toChunks

fromStrict :: BS.ByteString -> BL.ByteString
fromStrict = BL.fromChunks . return
#endif
