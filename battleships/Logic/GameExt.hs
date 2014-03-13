----------------------------------------------------------------------------
-- |
-- Module      :  Logic.GameExt
-- Stability   :  experimental
-- Portability :  semi-portable
--
-- External encrypted representation of the game state.

module Logic.GameExt
  ( impGame
  , expGame
  , loadKey
  , GameStateExt
  , DefaultAI
  ) where

import           Prelude
import           Logic.Game
import           Logic.Binary
import           Logic.CleverAI (CleverAI)
import           Yesod (PathPiece (..))
import           Codec.Crypto.SimpleAES
import           Control.Monad.IO.Class
import           Data.Serialize (Serialize, encode, decode)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL

-- | External encrypted game state representation.
data GameStateExt = GameStateExt
  { fromStateExt :: BL.ByteString } deriving (Eq, Show, Read)

-- | The AI we use by default.
-- This way, only one line of code needs to be changed
-- when switching to a different AI.
type DefaultAI = CleverAI

-------------------------------------------------------------------------------
-- * Conversions
-------------------------------------------------------------------------------

-- | Imports a game, given the key.
impGame :: Serialize a => Key -> GameStateExt -> Either String (GameState a)
impGame key game = decode dec where
  enc = fromStateExt game
  dec = BL.toStrict $ decryptMsg CBC key enc

-- | Exports a game, given the key.
expGame
  :: (MonadIO m, Serialize a)
  => Key -> GameState a -> m GameStateExt
expGame key game = liftIO $ do
  let dec = BL.fromStrict $ encode game
  enc <- encryptMsg CBC key dec
  return $ GameStateExt enc

-- | Loads the AES key.
loadKey :: String -> IO Key 
loadKey = BS.readFile

-------------------------------------------------------------------------------
-- * Path Piece
-------------------------------------------------------------------------------

instance PathPiece GameStateExt where
  fromPathPiece = fmap GameStateExt . impBinary
  toPathPiece   = expBinary . fromStateExt