{-# LANGUAGE RecordWildCards, TupleSections, OverloadedStrings #-}
module Logic.Game where

import Prelude
import Data.Array
import Data.Aeson hiding (Array)
import Data.Attoparsec.Number
import Data.Maybe
import Control.Monad
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
  { rulesSize         :: (Int, Int)
  , rulesShips        :: [Int]
  , rulesSafetyMargin :: Int
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
  { playerImpact :: TrackingGrid  -- ^ impacts on the player field
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

instance Random Orientation where
  randomR (a, b) g = (toEnum r, g') where
    (r, g')        = randomR (fromEnum a, fromEnum b) g
  random g         = randomR (Horizontal, Vertical) g

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
  let impacts  = (newGrid (rulesSize r) Nothing, Nothing)
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
  rangeCheck     = L.all (inRange range)
                 $ shipCoordinates 0 ship
  freeCheck      = L.all (isNothing . shipAt fleet)
                 $ shipCoordinates rulesSafetyMargin ship
  (w, h)         = rulesSize
  range          = ((0, 0), (w - 1, h - 1))

-- | Calculates the position occupied by a ship including safety margin.
shipCoordinates :: Int -> Ship -> [Pos]
shipCoordinates margin Ship{..} = case shipOrientation of
  Horizontal -> [(x + i, y + d) | i <- [-margin..shipSize - 1 + margin], d <- [-margin..margin]]
  Vertical   -> [(x + d, y + i) | i <- [-margin..shipSize - 1 + margin], d <- [-margin..margin]]
  where (x, y) = shipPosition

shipAt :: Fleet -> Pos -> Maybe Ship
shipAt fleet (px, py) = listToMaybe $ filter containsP fleet where
  containsP Ship {..} = case shipOrientation of
    Horizontal -> px >= sx && px < sx + shipSize && py == sy
    Vertical   -> px == sx && py >= sy && py < sy + shipSize
    where (sx, sy) = shipPosition

fireAt :: Fleet -> TrackingGrid -> Pos -> HitResponse
fireAt fleet impacts pos = case shipAt fleet pos of
  Nothing -> Water
  Just s  -> case leftover of
    []    -> Sunk
    _     -> Hit
    where leftover = filter (\p -> p /= pos && isNothing ((fst impacts) ! p)) $ shipCoordinates 0 s

allSunk :: Fleet -> TrackingGrid -> Bool
allSunk fleet impacts = foldr (&&) True hit where
  hit    = fmap (\p -> isJust ((fst impacts) ! p)) points
  points = fleet >>= shipCoordinates 0

isDamaged :: TrackingGrid -> Ship -> Bool
isDamaged impacts ship = case damaged of
  [] -> False
  _  -> True 
  where damaged = filter (\p -> isJust ((fst impacts) ! p)) $ shipCoordinates 0 ship

-------------------------------------------------------------------------------
-- * Turn
-------------------------------------------------------------------------------

data Turn a = Won (GameState a) | Lost (GameState a) | Next (GameState a)

turn :: (MonadRandom m, AI a) => GameState a -> Pos -> m (Turn a)
turn game pos = turnPlayer game pos >>= \t -> case t of
  Next game' -> turnEnemy game'
  _          -> return t

turnEnemy :: (MonadRandom m, AI a) => GameState a -> m (Turn a)
turnEnemy g = do
  (pos, s) <- runStateT aiFire (enemyState g)
  let (newState', response) = turnCommon g pos
  (_, s')  <- runStateT (aiResponse pos response) s
  let newState = newState' { enemyState = s' }
  return $ case allSunk (playerFleet newState) (playerImpact newState) of
    True  -> Lost newState
    False -> Next newState

turnPlayer :: (MonadRandom m, AI a) => GameState a -> Pos -> m (Turn a)
turnPlayer g pos = do
  let (newState, _) = turnCommon (switchRoles g) pos
  return $ case allSunk (playerFleet newState) (playerImpact newState) of
    True  -> Won (switchRoles newState)
    False -> Next (switchRoles newState)

-- Assumes it is the computer's turn. If it is the player's turn, you have to switch positions before and afterwards!
turnCommon :: AI a => GameState a -> Pos-> (GameState a, HitResponse)
turnCommon g@(GameState {..}) pos =
  let response = fireAt playerFleet playerImpact pos
      impact   = ((fst playerImpact) // [(pos, Just response)], Just pos)
  in (g { playerImpact = impact }, response)

switchRoles :: GameState a -> GameState a
switchRoles g = g
  { playerImpact = playerTrack g
  , playerTrack = playerImpact g
  , playerFleet = enemyFleet g
  , enemyFleet = playerFleet g
  }

-------------------------------------------------------------------------------
-- * Move a ship
-------------------------------------------------------------------------------

data Movement = Forward | Backward -- +1 or -1 for the respective coordinate

-- Tries to move the player's ship if pos is one of its endings.
move :: (MonadRandom m, AI a) => GameState a -> Pos -> m (Turn a)
move g@GameState{..} pos = case desiredMove playerFleet pos of
  Nothing               -> return $ Next g
  Just (ship, movement) -> if isDamaged playerImpact ship
                           then return $ Next g 
                           else return $ Next (g {playerFleet = newFleet}) 
                             where newFleet = moveShip playerFleet ship movement gameRules 

-- Find out which ship the player wants to move into which direction.
desiredMove :: Fleet -> Pos -> Maybe (Ship, Movement)
desiredMove fleet pos = case shipAt fleet pos of
  Nothing -> Nothing
  Just ship@Ship{..} -> case shipOrientation of
    Horizontal 
      | pos == (x, y)                -> Just (ship, Forward)
      | pos == (x + shipSize - 1, y) -> Just (ship, Backward)
    Vertical
      | pos == (x, y)                -> Just (ship, Forward)
      | pos == (x, y + shipSize - 1) -> Just (ship, Backward)
    _ -> Nothing
    where 
      (x,y) = shipPosition

-- Only moves the ship if it complies with the given rules.
moveShip :: Fleet -> Ship -> Movement -> Rules -> Fleet
moveShip fleet ship movement rules = case shipAdmissible rules (filter (/= ship) fleet) newShip of
  False -> fleet
  True  -> substituteShipBy fleet ship newShip
  where newShip = movedShip ship movement

-- Ship after movement was made.
movedShip :: Ship -> Movement -> Ship
movedShip ship movement = case (shipOrientation ship, movement) of
  (Horizontal, Forward)  -> ship {shipPosition = (x - 1, y)}
  (Horizontal, Backward) -> ship {shipPosition = (x + 1, y)}
  (Vertical, Forward)    -> ship {shipPosition = (x, y - 1)}
  (Vertical, Backward)   -> ship {shipPosition = (x, y + 1)}
  where (x,y) = shipPosition ship

substituteShipBy :: Fleet -> Ship -> Ship -> Fleet
substituteShipBy fleet oldShip newShip = newShip : filter (/= oldShip) fleet

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
  get = Rules <$> get <*> get <*> get
  put Rules {..} = put rulesSize >> put rulesShips >> put rulesSafetyMargin

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


-------------------------------------------------------------------------------
-- * JSON
-------------------------------------------------------------------------------
instance FromJSON Ship where
   parseJSON (Object v) = curry Ship <$>
                          v .: "X" <*>
                          v .: "Y" <*>
                          v .: "Size" <*>
                          v .: "Orientation"
   -- A non-Object value is of the wrong type, so fail.
   parseJSON _          = mzero
instance FromJSON Orientation where
  parseJSON (Number (I i)) = return $ toEnum $ fromIntegral i
  parseJSON (Number (D d)) = return $ toEnum $ floor d
  parseJSON _              = mzero