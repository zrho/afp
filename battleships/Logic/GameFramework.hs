{-# LANGUAGE RecordWildCards #-}
module Logic.GameFramework
  ( AI (..)
  , Rules (..)
  , HitResponse (..)
  , Orientation (..)
  , Ship (..)
  , PlayerState (..)
  , Fleet
  , Pos
  , Grid
  , TrackingGrid
  , PlayerGrid
  , gridSize
  , shipCoordinates
  , shipAt
  ) where

import Prelude
import Data.Array
import Data.Maybe
import Data.Serialize (Serialize(..))
import Control.Applicative
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.State.Class (MonadState)

-------------------------------------------------------------------------------
-- * Type-Classes
-------------------------------------------------------------------------------

-- | Operations an artificial intelligence has to provide for playing this game.
-- The state is maintained across all calls of `aiPlaceFleet`, `aiFire` and `aiResponse`.
-- The initial state must be provided by `aiInit`.
class AI a where
  aiInit       :: MonadRandom m 
               => Rules -- ^ game rules
               -> m a   -- ^ monad for computing initial ai state, 
                        -- which is in turn feeded to the first call of aiFire
  -- | AI ship placement
  aiPlaceFleet :: (MonadRandom m, MonadState a m) 
               => [Int]   -- ^ available ships (stored as length)
               -> m Fleet
  -- | computes the next shot based on the current AI state
  aiFire       :: (MonadRandom m, MonadState a m) 
               => m Pos
  -- | Feedback to `aiFire`
  aiResponse   :: (MonadRandom m, MonadState a m) 
               => (Pos, HitResponse) -- ^ the target position and result of the last shot
               -> m ()

-------------------------------------------------------------------------------
-- * Data Types
-------------------------------------------------------------------------------

data Rules = Rules { mapSize :: (Int, Int) }

-- | Reponse sent to the AI after a shot.
data HitResponse = Water -- ^ the shot hit the water
                 | Hit   -- ^ the shot hit a ship
                 | Sunk  -- ^ the shot hit the last intact part of a ship
                 deriving (Show, Eq, Ord, Bounded, Enum)

data Orientation = Horizontal | Vertical deriving (Show, Eq, Ord, Bounded, Enum)

data Ship = Ship 
            { shipPosition    :: Pos
            , shipSize        :: Int
            , shipOrientation :: Orientation
            } deriving (Show, Eq)

-- | Information about a player
data PlayerState = PlayerState 
                   { -- | the player's ships
                     playerFleet :: Fleet
                     -- | the grid where the impacts on this player's map are marked
                   , playerGrid  :: PlayerGrid
                     -- | the grid which contains the player's information about the enemy fleet
                   , enemyGrid   :: TrackingGrid
                   }

-------------------------------------------------------------------------------
-- * Type Synonyms
-------------------------------------------------------------------------------

-- | A fleet is just a list of ships
type Fleet = [Ship]

-- | A two-dimensional position stored with zero-based indices
type Pos = (Int,Int)

-- | A grid is an array indexed by positions with zero-based indices.
type Grid a = Array Pos a

-- | A grid where the results of shots are tracked
-- Indices must be zero based to guarantee consistency.
type TrackingGrid = Grid (Maybe HitResponse)

-- | A grid where the impacts of shots are tracked.
-- True means, the cell was hit.
type PlayerGrid   = Grid Bool


-------------------------------------------------------------------------------
-- * Helper Functions
-------------------------------------------------------------------------------

gridSize :: Grid a -> (Int, Int)
gridSize grid = let ((x1,y1),(x2,y2)) = bounds grid in (x2 - x1 + 1, y2 - y1 + 1)

shipCoordinates :: Ship -> [Pos]
shipCoordinates ship = 
  let (x,y) = shipPosition ship
  in case shipOrientation ship of
    Horizontal -> [(x + i, y) | i <- [0..shipSize ship - 1]]
    Vertical   -> [(x, y + i) | i <- [0..shipSize ship - 1]]


shipAt :: Fleet -> Pos -> Maybe Ship
shipAt fleet (px,py) = listToMaybe $ filter (containsP) fleet where
  containsP ship = 
    let (sx, sy) = shipPosition ship 
    in case shipOrientation ship of
      Horizontal -> px >= sx && px < sx + shipSize ship && py == sy
      Vertical   -> px == sx && py >= sy && py < sy + shipSize ship


-------------------------------------------------------------------------------
-- * Serialization
-------------------------------------------------------------------------------

instance Serialize PlayerState where
  get = PlayerState <$> get <*> get <*> get
  put PlayerState {..} =  put playerFleet 
                       >> put playerGrid
                       >> put enemyGrid

instance Serialize Rules where
  get = Rules <$> get
  put Rules {..} = put mapSize

instance Serialize HitResponse where
  get = toEnum <$> get
  put = put . fromEnum

instance Serialize Orientation where
  get = toEnum <$> get
  put = put . fromEnum

instance Serialize Ship where
  get = Ship <$> get <*> get <*> get
  put Ship{..} =  put shipPosition
               >> put shipSize
               >> put shipOrientation
