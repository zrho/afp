module Logic.GameExt where

import           Prelude
import           Logic.Game
import           Yesod (PathPiece (..))
import           Codec.Crypto.SimpleAES
import           Control.Monad.IO.Class
import           Data.Serialize (Serialize, encode, decode)
import qualified Data.Text.Encoding          as TE
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString.Base64.Lazy as B64

data GameStateExt = GameStateExt
  { fromStateExt :: BL.ByteString } deriving (Eq, Show, Read)

-------------------------------------------------------------------------------
-- * Conversions
-------------------------------------------------------------------------------

-- | Imports a game.
impGame :: (MonadIO m, Serialize a) => GameStateExt -> m (Maybe (GameState a))
impGame game = liftIO $ do
  key <- loadKey
  let enc = fromStateExt game
  let dec = toStrict $ decryptMsg CBC key enc
  return $ eitherToMaybe $ decode dec

-- | Exports a game.
expGame :: (MonadIO m, Serialize a) => GameState a -> m GameStateExt
expGame game = liftIO $ do
  key <- loadKey
  let dec = fromStrict $ encode game
  enc <- encryptMsg CBC key dec
  return $ GameStateExt enc

-- | Loads the AES key.
loadKey :: IO Key 
loadKey = BS.readFile "key.aes"

-------------------------------------------------------------------------------
-- * Path Piece
-------------------------------------------------------------------------------

instance PathPiece GameStateExt where
  fromPathPiece
    = fmap GameStateExt
    . eitherToMaybe
    . B64.decode
    . fromStrict
    . TE.encodeUtf8

  toPathPiece
    = TE.decodeUtf8
    . toStrict
    . B64.encode
    . fromStateExt

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe e = case e of
  Right x -> Just x
  _       -> Nothing

toStrict = BS.concat . BL.toChunks
fromStrict = BL.fromChunks . return