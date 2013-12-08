module Logic.GameState
  ( GameState (..)
  , ExtGameState
  , impGame
  , expGame
  ) where

import           Prelude
import           Yesod hiding (Key)
import Codec.Crypto.SimpleAES
import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Int
import           Data.Serialize (Serialize, encode, decode)
import qualified Data.Serialize              as S
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString.Base16.Lazy as B16

-------------------------------------------------------------------------
-- * Game State

-- | Internal game representation.
data GameState = GameState
  { gameRange   :: (Int, Int)
  , gameAnswer  :: Int
  , gameHistory :: [Int]
  } deriving (Eq, Show)

instance Serialize GameState where
  put (GameState r a h) = S.put r >> S.put a >> S.put h
  get                   = GameState <$> S.get <*> S.get <*> S.get

-------------------------------------------------------------------------
-- ** External representation

-- | External game representation.
newtype ExtGameState = ExtGameState
  { fromExtGameState :: BL.ByteString }
  deriving (Eq, Show, Read)

-- | Imports a game.
impGame :: MonadIO m => ExtGameState -> m (Maybe GameState)
impGame game = liftIO $ do
  key <- loadKey
  let enc = fromExtGameState game
  let dec = toStrict $ decryptMsg CBC key enc
  return $ eitherToMaybe $ decode dec

-- | Exports a game.
expGame :: MonadIO m => GameState -> m ExtGameState
expGame game = liftIO $ do
  key <- loadKey
  let dec = fromStrict $ encode game
  enc <- encryptMsg CBC key dec
  return $ ExtGameState enc

-- | Loads the AES key.
loadKey :: IO Key 
loadKey = BS.readFile "config/key.aes"

-------------------------------------------------------------------------
-- ** Yesod compatibility

instance PathPiece ExtGameState where
  fromPathPiece
    = fmap ExtGameState
    . checkValidity
    . B16.decode
    . fromStrict
    . TE.encodeUtf8

  toPathPiece
    = TE.decodeUtf8
    . toStrict
    . B16.encode
    . fromExtGameState

-------------------------------------------------------------------------
-- * Utilities

checkValidity :: (BL.ByteString, BL.ByteString) -> Maybe BL.ByteString
checkValidity (x,y) = if BL.null y then Just x else Nothing

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe e = case e of
  Right x -> Just x
  _       -> Nothing

toStrict :: BL.ByteString -> BS.ByteString
toStrict = BS.concat . BL.toChunks

fromStrict :: BS.ByteString -> BL.ByteString
fromStrict s = BL.fromChunks [s]
