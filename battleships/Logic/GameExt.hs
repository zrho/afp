{-# LANGUAGE TypeFamilies, RecordWildCards #-}
module Logic.GameExt
  ( impGame
  , expGame
  , loadKey
  , GameStateExt
  ) where

import           Prelude
import           Logic.Game
import           Logic.Binary
import           Yesod (PathPiece (..))
import           Codec.Crypto.SimpleAES
import           Control.Monad.IO.Class
import           Data.Serialize (Serialize, encode, decode)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL

data GameStateExt = GameStateExt
  { fromStateExt :: BL.ByteString } deriving (Eq, Show, Read)

-------------------------------------------------------------------------------
-- * Conversions
-------------------------------------------------------------------------------

-- | Imports a game, given the key.
impGame :: Serialize a => Key -> GameStateExt -> Maybe (GameState a)
impGame key game = eitherToMaybe $ decode dec where
  enc = fromStateExt game
  dec = toStrict $ decryptMsg CBC key enc

-- | Exports a game, given the key.
expGame
  :: (MonadIO m, Serialize a)
  => Key -> GameState a -> m GameStateExt
expGame key game = liftIO $ do
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
  fromPathPiece = fmap GameStateExt . impBinary
  toPathPiece   = expBinary . fromStateExt