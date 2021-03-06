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
  ) where

import           Prelude
import           Codec.Crypto.SimpleAES
import           Control.Monad.IO.Class
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import           Data.Serialize (Serialize, encode, decode)
import           Logic.Binary
import           Logic.Types
import           Yesod (PathPiece (..))

-- | External encrypted game state representation.
data GameStateExt = GameStateExt
  { fromStateExt :: BL.ByteString } deriving (Eq, Show, Read)

-------------------------------------------------------------------------------
-- * Conversions
-------------------------------------------------------------------------------

-- | Imports a game, given the key.
impGame :: Serialize a => Key -> GameStateExt -> Either String (GameState a)
impGame key game = decode dec where
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
loadKey :: String -> IO Key 
loadKey = BS.readFile

-------------------------------------------------------------------------------
-- * Path Piece
-------------------------------------------------------------------------------

instance PathPiece GameStateExt where
  fromPathPiece = fmap GameStateExt . impBinary
  toPathPiece   = expBinary . fromStateExt

instance PathPiece Bool where
    fromPathPiece "1" = Just True
    fromPathPiece "0" = Just False
    fromPathPiece _   = Nothing
    toPathPiece b = if b then "1" else "0"
