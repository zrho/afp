----------------------------------------------------------------------------
-- |
-- Module      :  Logic.Types
-- Stability   :  experimental
-- Portability :  semi-portable
--
-- Data structures and type classes of the battleships implementation.
--
module Logic.Types
  (
  -- * Typeclasses
    AI (..)
  , HasShipShape (..)
  -- * Datatypes
  , Action (..)
  , GameState (..)
  , DifficultyLevel (..)
  , HitResponse (..)
  , Movement (..)
  , Orientation (..)
  , Player (..)
  , PlayerState (..)
  , Options (..)
  , Rules (..)
  , Ship (..)
  , ShipShape (..)
  , ShipMove (..)
  , Shot (..)
  , Turn (..)
  -- * Type Synonyms
  , Fleet
  , FleetPlacement
  , Grid
  , Pos
  , ShipID
  , TrackingList
  -- * Smart Constructors
  , newGrid
  -- * Serialization Helpers
  , getPos
  , getSmallGrid
  , putPos
  , putSmallGrid
  ) where


import           Prelude
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.State.Class (MonadState)
import           Data.Aeson hiding (Array, encode, decode)
import           Data.Array
import           Data.Bits
import           Data.Function (on)
import qualified Data.Map as Map
import           Data.Serialize (Serialize (..), encode, decode)
import           Data.Serialize.Get
import           Data.Serialize.Put
import           Data.Word
import           Logic.Binary
import           Yesod (PathPiece (..))

-------------------------------------------------------------------------------
-- * AI
-------------------------------------------------------------------------------

-- | Operations an artificial intelligence has to provide for playing this game.
class AI a where
  -- | Initial state and fleet position
  aiInit
    :: MonadRandom m 
    => Rules                 -- ^ game rules
    -> m (a, FleetPlacement) -- ^ initial state

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

  -- | Computes the ship movement
  aiMove
    :: (MonadRandom m, MonadState a m)
    => Fleet                        -- ^ AI's fleet including IDs
    -> TrackingList                 -- ^ enemy shots
    -> m (Maybe (ShipID, Movement)) -- ^ ship and movement, if any
  aiMove _ _ = return Nothing

-------------------------------------------------------------------------------
-- * Game State
-------------------------------------------------------------------------------

data Options = Options
  { againWhenHit   :: Bool
  , move           :: Bool
  , noviceMode     :: Bool
  , devMode        :: Bool
  , difficulty     :: DifficultyLevel
  } deriving (Show, Eq, Read)

data Rules = Rules
  { rulesAgainWhenHit   :: Bool
  , rulesMove           :: Bool
  , rulesDifficulty     :: DifficultyLevel
  , rulesMaximumTurns   :: Int
  , rulesCountdownTurns :: Int
  }

-- | Playing strength of the AI.
data DifficultyLevel
  = Easy
  | Medium
  | Hard
  deriving (Enum, Eq, Show, Read)

-- | Reponse sent to the AI after a shot.
data HitResponse
  = Water -- ^ the shot hit the water
  | Hit   -- ^ the shot hit a ship
  | Sunk  -- ^ the shot hit the last intact part of a ship
  deriving (Eq, Enum)

data Orientation 
  = Horizontal
  | Vertical
  deriving (Enum, Bounded)

-- | Encodes a ships state using position, size and orientation
data ShipShape = ShipShape 
  { shipPosition    :: Pos
  , shipSize        :: Int
  , shipOrientation :: Orientation
  }

-- | A ship with unique id, a shape and current damage.
data Ship = Ship
  { shipID     :: ShipID         -- ^ unique ID of ship
  , shipShape  :: ShipShape      -- ^ shape of ship (including position, orientation and size)
  , shipDamage :: Array Int Bool -- ^ damage at each position
  }

instance Eq Ship where
  (==) = (==) `on` shipID

-- | Used to allow lookup functions to work with both FleetPlacement and Fleet
class HasShipShape a where
  getShipShape :: a -> ShipShape
instance HasShipShape Ship where
  getShipShape = shipShape
instance HasShipShape ShipShape where
  getShipShape = id

-- | State of the game for both players.
data GameState a = GameState
  { currentPlayer  :: PlayerState -- ^ the current player's state 
  , otherPlayer    :: PlayerState -- ^ the other player's state
  , aiState        :: a           -- ^ state of the AI
  , gameRules      :: Rules
  , noviceModeOpt  :: Bool
  , devModeOpt     :: Bool
  , expectedAction :: Action
  , turnNumber     :: Int
  }

-- | The state belonging to one player
data PlayerState = PlayerState
  { playerShots :: TrackingList -- ^ the player's shots
  , playerFleet :: Fleet        -- ^ the player's fleet
  , playerType  :: Player
  , playerMoves :: [ShipMove]   -- ^ the player's moves
  } deriving Eq

-- | type to distinguish between human and AI player
data Player
  = HumanPlayer
  | AIPlayer
  deriving (Eq, Enum)

-- | the next action expected from the human player
data Action
  = ActionFire
  | ActionMove
  deriving (Eq, Enum)

-- | keeps track of all relevant information for a fired shot
data Shot = Shot
  { shotPos    :: Pos
  , shotResult :: HitResponse
  , shotTime   :: Int
  } deriving Eq

-- | a ship movement
data ShipMove = ShipMove
  { shipMoveID        :: ShipID
  , shipMoveDirection :: Movement
  , shipMoveTime      :: Int
  } deriving (Eq, Show)

-- | A fleet is a map of ship ID's to ships
type Fleet = Map.Map ShipID Ship

-- | A fleet placement is a list of the ship shapes
type FleetPlacement = [ShipShape]

-- | A two-dimensional position stored with zero-based indices
type Pos = (Int,Int)

-- | A grid is an array indexed by positions with zero-based indices
type Grid a = Array Pos a

-- | A list of all shots. Easier to handle, because no index lookup is needed.
type TrackingList = [Shot]

-- | Used for identifying a ship.
type ShipID = Int

-- | Result of the turn with respect to the current player
data Turn = Over | Again | Next deriving Eq

-- | A ships movement
data Movement 
  = Forward  -- ^ -1 for the respective coordinate
  | Backward -- ^ +1 for the respective coordinate
  deriving (Eq, Enum, Bounded, Show)

-------------------------------------------------------------------------------
-- * Smart Constructors
-------------------------------------------------------------------------------

-- | Creates a grid, filled with one value.
newGrid :: (Int, Int) -> a -> Grid a
newGrid (w, h) a
  = array ((0, 0), (w - 1, h - 1))
    [((x, y), a) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

-------------------------------------------------------------------------------
-- * Random
-------------------------------------------------------------------------------

instance Random Orientation where
  randomR (a, b) g = (toEnum r, g') where
    (r, g')        = randomR (fromEnum a, fromEnum b) g
  random           = randomR (minBound, maxBound)

instance Random Movement where
  randomR (a, b) g = (toEnum r, g') where
    (r, g')        = randomR (fromEnum a, fromEnum b) g
  random           = randomR (minBound, maxBound)

-------------------------------------------------------------------------------
-- * JSON
-------------------------------------------------------------------------------

instance FromJSON ShipShape where
   parseJSON (Object v) = curry ShipShape <$>
                          v .: "X" <*>
                          v .: "Y" <*>
                          v .: "Size" <*>
                          v .: "Orientation"
   -- A non-Object value is of the wrong type, so fail.
   parseJSON _          = mzero

instance FromJSON Orientation where
  parseJSON (Number n) = return $ toEnum $ floor n
  parseJSON _          = mzero

instance ToJSON Orientation where
  toJSON = Number . fromIntegral . fromEnum

instance ToJSON ShipShape where
  toJSON (ShipShape {..}) = object 
    [ "X"           .= fst shipPosition
    , "Y"           .= snd shipPosition
    , "Size"        .= shipSize
    , "Orientation" .= shipOrientation
    ]

-------------------------------------------------------------------------------
-- * Path Pieces
-------------------------------------------------------------------------------

instance PathPiece Options where
  fromPathPiece = impBinary >=> eitherToMaybe . decode . toStrict
  toPathPiece   = expBinary . fromStrict . encode

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe e = case e of
  Right x -> Just x
  _       -> Nothing

-------------------------------------------------------------------------------
-- * Serialization
-------------------------------------------------------------------------------

instance Serialize a => Serialize (GameState a) where
  get = GameState <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> getIntegral8
  put GameState {..} = do
    put currentPlayer
    put otherPlayer
    put aiState
    put gameRules
    put noviceModeOpt
    put devModeOpt
    put expectedAction
    putIntegral8 turnNumber

instance Serialize PlayerState where
  get = PlayerState <$> getList8 get <*> get <*> get <*> get
  put PlayerState{..} = do
    putList8 put playerShots
    put playerFleet
    put playerType
    put playerMoves

instance Serialize Options where
  get = Options <$> get <*> get <*> get <*> get <*> get
  put Options {..} = do
    put againWhenHit
    put move
    put noviceMode
    put devMode
    put difficulty

instance Serialize Rules where
  get = Rules <$> get <*> get <*> get <*> getIntegral8 <*> getIntegral8
  put Rules {..} = do
    put rulesAgainWhenHit
    put rulesMove
    put rulesDifficulty
    putIntegral8 rulesMaximumTurns
    putIntegral8 rulesCountdownTurns

instance Serialize DifficultyLevel where
  get = getEnum8
  put = putEnum8

instance Serialize HitResponse where
  get = getEnum8
  put = putEnum8

instance Serialize Orientation where
  get = getEnum8
  put = putEnum8

instance Serialize Player where
  get = getEnum8
  put = putEnum8

instance Serialize Action where
  get = getEnum8
  put = putEnum8

instance Serialize Movement where
  get = getEnum8
  put = putEnum8

instance Serialize ShipShape where
  get = ShipShape <$> getPos <*> getIntegral8 <*> get
  put ShipShape{..} = do
    putPos shipPosition
    putIntegral8 shipSize
    put shipOrientation

instance Serialize Ship where
  get = Ship <$> getIntegral8 <*> get <*> get
  put Ship{..} = do
    putIntegral8 shipID
    put shipShape
    put shipDamage

instance Serialize Shot where
  get = Shot <$> getPos <*> get <*> getIntegral8
  put Shot{..} = do
    putPos shotPos
    put shotResult
    putIntegral8 shotTime

instance Serialize ShipMove where
  get = ShipMove <$> getIntegral8 <*> get <*> getIntegral8
  put ShipMove{..} = do
    putIntegral8 shipMoveID
    put shipMoveDirection
    putIntegral8 shipMoveTime


-------------------------------------------------------------------------------
-- * Specific Serialization Helpers
-------------------------------------------------------------------------------

putPos :: Putter Pos
putPos (x,y) = put (hi .|. lo :: Word8) where
  hi = (fromIntegral x .&. 0x0F) `shiftL` 4
  lo =  fromIntegral y .&. 0x0F

getPos :: Get Pos
getPos = fromB <$> getWord8 where
  fromB b = ( fromIntegral $ b `shiftR` 4
            , fromIntegral $ b .&. 0x0F   )

putSmallGrid :: Putter a -> Putter (Grid a)
putSmallGrid pel grid = do
  let (_, upper) = bounds grid
  putPos upper
  mapM_ pel (elems grid)

getSmallGrid :: Get a -> Get (Grid a)
getSmallGrid gel = do
  upper <- getPos
  let count = rangeSize ((0,0), upper)
  xs <- replicateM count gel
  return $ listArray ((0,0), upper) xs
