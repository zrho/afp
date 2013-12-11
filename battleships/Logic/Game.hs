{-# LANGUAGE RecordWildCards, TupleSections #-}
module Logic.Game where

import Prelude
import Data.Array
import Data.Maybe
import Data.Serialize (Serialize (..))
import Data.Int
import Data.List as L
import Control.Applicative
import Control.Monad.Random
import Control.Monad.Trans.State (runStateT)
import Control.Monad.State.Class (MonadState)

-------------------------------------------------------------------------------
-- * AI
-------------------------------------------------------------------------------

-- | Operations an artificial intelligence has to provide for playing this game.
class AI a where
  -- | Initial state and fleet position
  aiInit
    :: MonadRandom m 
    => Rules          -- ^ game rules
    -> m (a, Fleet)   -- ^ initial state

  -- | Computes the next shot based on the current AI state
  aiFire
    :: (MonadRandom m, MonadState a m) 
    => m Pos

  -- | Feedback to `aiFire`
  aiResponse
    :: (MonadRandom m, MonadState a m) 
    => Pos         -- ^ target position
    -> HitResponse -- ^ result of the last shot
    -> m ()

-------------------------------------------------------------------------------
-- * Game State
-------------------------------------------------------------------------------

data Rules = Rules
  { rulesSize  :: (Int, Int)
  , rulesShips :: [Int]
  }

-- | Reponse sent to the AI after a shot.
data HitResponse
  = Water -- ^ the shot hit the water
  | Hit   -- ^ the shot hit a ship
  | Sunk  -- ^ the shot hit the last intact part of a ship
  deriving (Show, Eq, Ord, Bounded, Enum)

data Orientation 
  = Horizontal
  | Vertical
  deriving (Show, Eq, Ord, Bounded, Enum)

data Ship = Ship 
  { shipPosition    :: Pos
  , shipSize        :: Int
  , shipOrientation :: Orientation
  } deriving (Show, Eq)

-- | State of the game for both players.
data GameState a = GameState
  { playerImpact :: ImpactGrid    -- ^ impacts on the player field
  , playerTrack  :: TrackingGrid  -- ^ tracking for the player
  , playerFleet  :: Fleet         -- ^ fleet of the player
  , enemyFleet   :: Fleet         -- ^ fleet of the enemy
  , enemyState   :: a             -- ^ state of the enemy
  , gameRules    :: Rules
  }

-- | A fleet is a list of ships
type Fleet = [Ship]

-- | A two-dimensional position stored with zero-based indices
type Pos = (Int,Int)

-- | A grid is an array indexed by positions with zero-based indices
type Grid a = Array Pos a

-- | A grid where the results of shots and the position of the last shot are tracked.
type TrackingGrid = (Grid (Maybe HitResponse), Maybe Pos)

-- | A grid where the impacts of shots and the position of the last shot are tracked..
type ImpactGrid = (Grid Bool, Maybe Pos)

-------------------------------------------------------------------------------
-- * New Games
-------------------------------------------------------------------------------

-- | Creates a new game.
newGame
  :: (AI a, MonadRandom m)
  => Rules  -- ^ rules of the game
  -> Fleet  -- ^ fleet of the human player
  -> m (GameState a)

newGame r pFleet = do 
  (ai, eFleet) <- aiInit r
  let impacts  = (newGrid (rulesSize r) False, Nothing)
  let tracking = (newGrid (rulesSize r) Nothing, Nothing)
  return $ GameState impacts tracking pFleet eFleet ai r

-- | Helper: Creates a grid, filled with one value.
newGrid :: (Int, Int) -> a -> Grid a
newGrid (w, h) a
  = array ((0, 0), (w - 1, h - 1))
    [((x, y), a) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

-------------------------------------------------------------------------------
-- * Helper Functions
-------------------------------------------------------------------------------

gridSize :: Grid a -> (Int, Int)
gridSize grid = let ((x1,y1),(x2,y2)) = bounds grid in (x2 - x1 + 1, y2 - y1 + 1)

shipAdmissible :: Rules -> Fleet -> Ship -> Bool
shipAdmissible (Rules {..}) fleet ship = rangeCheck && freeCheck where
  rangeCheck     = L.all (inRange range) shipCoords
  freeCheck      = L.all (isNothing . shipAt fleet) shipCoords
  shipCoords     = shipCoordinates ship
  (w, h)         = rulesSize
  range          = ((0, 0), (w - 1, h - 1))

shipCoordinates :: Ship -> [Pos]
shipCoordinates Ship {..} = case shipOrientation of
  Horizontal -> [(x + i, y) | i <- [0..shipSize - 1]]
  Vertical   -> [(x, y + i) | i <- [0..shipSize - 1]]
  where (x, y) = shipPosition

shipAt :: Fleet -> Pos -> Maybe Ship
shipAt fleet (px, py) = listToMaybe $ filter containsP fleet where
  containsP Ship {..} = case shipOrientation of
    Horizontal -> px >= sx && px < sx + shipSize && py == sy
    Vertical   -> px == sx && py >= sy && py < sy + shipSize
    where (sx, sy) = shipPosition

fireAt :: Fleet -> ImpactGrid -> Pos -> HitResponse
fireAt fleet impacts pos = case shipAt fleet pos of
  Nothing -> Water
  Just s  -> case leftover of
    []    -> Sunk
    _     -> Hit
    where leftover = filter (\p -> not $ p == pos || (fst impacts) ! p) $ shipCoordinates s

allSunk :: Fleet -> ImpactGrid -> Bool
allSunk fleet impacts = foldr (&&) True hit where
  hit    = fmap ((fst impacts) !) points
  points = fleet >>= shipCoordinates

trackToImpact :: TrackingGrid -> ImpactGrid
trackToImpact t = (fmap (/= Nothing) $ fst t, snd t)

-------------------------------------------------------------------------------
-- * Turn
-------------------------------------------------------------------------------

data Turn a = Won (GameState a) | Lost (GameState a) | Next (GameState a)

turn :: (MonadRandom m, AI a) => GameState a -> Pos -> m (Turn a)
turn game pos = turnPlayer game pos >>= \t -> case t of
  Next game' -> return $ Next game' -- turnEnemy game'
  _          -> return t

turnEnemy :: (MonadRandom m, AI a) => GameState a -> m (Turn a)
turnEnemy g@(GameState {..}) = do
  -- determine next position
  (pos, s) <- runStateT aiFire enemyState
  -- fire the enemy's shot
  let response = fireAt playerFleet playerImpact pos
  -- update the impact grid
  let impact   = ((fst playerImpact) // [(pos, True)], Just pos)
  -- notify the AI
  (_, s') <- runStateT (aiResponse pos response) s
  -- all player ships sunk now?
  return $ case allSunk playerFleet impact of
    True  -> Lost $ g { playerImpact = impact, enemyState = s' }
    False -> Next $ g { playerImpact = impact, enemyState = s' }

turnPlayer :: Monad m => GameState a -> Pos -> m (Turn a)
turnPlayer g@(GameState {..}) pos = do
  -- fire the player's shot
  let response  = fireAt enemyFleet (trackToImpact playerTrack) pos
  -- update the tracking grid
  let track     = ((fst playerTrack) // [(pos, Just response)], Just pos)
  -- all enemy ships sunk now?
  return $ case allSunk enemyFleet (trackToImpact track) of
    True  -> Won $ g { playerTrack = track }
    False -> Next $ g { playerTrack = track }

-------------------------------------------------------------------------------
-- * Serialization
-------------------------------------------------------------------------------

instance Serialize a => Serialize (GameState a) where
  get = GameState <$> get <*> get <*> get <*> get <*> get <*> get
  put GameState {..} = do
    put playerImpact
    put playerTrack 
    put playerFleet 
    put enemyFleet
    put enemyState
    put gameRules

instance Serialize Rules where
  get = Rules <$> get <*> get
  put Rules {..} = put rulesSize >> put rulesShips

instance Serialize HitResponse where
  get = fromByte <$> get
  put = put . toByte

instance Serialize Orientation where
  get = fromByte <$> get
  put = put . toByte

instance Serialize Ship where
  get = Ship <$> get <*> get <*> get
  put Ship{..} = do
    put shipPosition
    put shipSize
    put shipOrientation

toByte :: Enum a => a -> Int8
toByte = fromIntegral . fromEnum

fromByte :: Enum a => Int8 -> a
fromByte = toEnum . fromIntegral