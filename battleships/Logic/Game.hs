{-# LANGUAGE RecordWildCards, TupleSections, OverloadedStrings #-}
module Logic.Game
  ( 
  -- * Typeclasses
    AI (..)
  -- * Datatypes
  , Action (..)
  , GameState (..)
  , HitResponse (..)
  , Movement (..)
  , Orientation (..)
  , Player (..)
  , PlayerState (..)
  , Rules (..)
  , Ship (..)
  , Ships (..)
  , ShipShape (..)
  , Shot (..)
  , Turn (..)
  -- * Type Synonyms
  , Fleet
  , FleetPlacement
  , Grid
  , Pos
  , TrackingList
  -- * Game Functions
  , boardSize
  , defaultRules
  , newGame
  , newGrid
  -- * Turn Functions
  , isDrawn
  , aiTurn
  , desiredMove
  , executeMove
  , humanTurnFire
  , moveHuman
  -- * Custom Serialization Functions
  , getIntegral8
  , getList8
  , getSmallGrid
  , getPos
  , putIntegral8
  , putList8
  , putPos
  , putSmallGrid
  -- * Ship Functions
  , allSunk 
  , damageShip
  , generateFleet
  , isDamaged
  , isMovable
  , isShipSunk
  , isShipAtSunk
  , moveShip
  , numberShipsOfSize
  , shipAdmissible
  , shipAt
  , shipCellIndex
  , shipCoordinates
  , shipSizes     -- TODO: think of better names for these two functions: shipSizes, sizesOfShips
  , sizesOfShips
  , unsunkShips
  ) where

import           Prelude hiding (and, or, foldl, foldr, mapM_)
import           Logic.Binary
import           Data.Array
import           Data.Aeson hiding (Array, encode, decode)
import           Data.Attoparsec.Number
import           Data.Maybe
import           Data.Foldable
import           Data.Function (on)
import           Control.Monad hiding (forM_, mapM_)
import           Data.Bits
import           Data.Serialize (Serialize (..), encode, decode)
import           Data.Serialize.Get
import           Data.Serialize.Put
import           Data.Word
import qualified Data.Map as Map
import           Data.List as L hiding (and, or, foldl, foldr, find)
import           Control.Applicative
import           Control.Monad.Random
import           Control.Monad.Trans.State (runStateT)
import           Control.Monad.State.Class (MonadState, gets, modify)
import           Yesod (PathPiece (..))
import qualified Data.Text as T hiding (find, zip, map)

-------------------------------------------------------------------------------
-- * Constants
-------------------------------------------------------------------------------

boardSize :: (Int, Int)
boardSize = (10, 10)

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

data Rules = Rules
  { rulesShips        :: [Int]
  , rulesAgainWhenHit :: Bool
  , rulesMove         :: Bool
  , rulesNoviceMode   :: Bool
  , rulesDevMode      :: Bool
  , rulesMaximumTurns :: Int
  } deriving (Show, Eq, Read)

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

-- | Encodes a ships state using position, size and orientation
data ShipShape = ShipShape 
  { shipPosition    :: Pos
  , shipSize        :: Int
  , shipOrientation :: Orientation
  } deriving (Show, Eq)

-- | A ship using with unique id, a shape and current damage.
data Ship = Ship
  { shipID     :: ShipID         -- ^ unique ID of ship
  , shipShape  :: ShipShape      -- ^ shape of ship (including position, orientation and size)
  , shipDamage :: Array Int Bool -- ^ damage at each position
  } deriving (Show)

instance Eq Ship where
  (==) = (==) `on` shipID

-- | Used to allow lookup functions to work both with FleetPlacement and Fleet
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
  , expectedAction :: Action
  , turnNumber     :: Int
  }

-- | The state belonging to one player
data PlayerState = PlayerState
  { playerShots :: TrackingList -- ^ the player's shots
  , playerFleet :: Fleet        -- ^ the player's fleet
  , playerType  :: Player
  , playerMoves :: [ShipMove]   -- ^ the player's moves
  } deriving (Show, Eq)

-- | type to distinguish between human and AI player
data Player
  = HumanPlayer
  | AIPlayer
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | the next action expected from the human player
data Action
  = ActionFire
  | ActionMove
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | keeps track of all relevant information for a fired shot
data Shot = Shot
  { shotPos    :: Pos
  , shotResult :: HitResponse
  , shotTime   :: Int
  } deriving (Show, Eq)

-- | a ship movement
data ShipMove = ShipMove
  { shipMoveID        :: ShipID
  , shipMoveDirection :: Movement
  , shipMoveTime      :: Int
  } deriving (Show, Eq)

instance PathPiece Action where
  fromPathPiece "ActionFire" = Just ActionFire
  fromPathPiece "ActionMove" = Just ActionMove
  fromPathPiece  _ = Nothing
  toPathPiece = T.pack . show

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

-------------------------------------------------------------------------------
-- * New Games
-------------------------------------------------------------------------------

-- | Creates a new game.
newGame
  :: (AI a, MonadRandom m)
  => Rules          -- ^ rules of the game
  -> FleetPlacement -- ^ fleet of the human player
  -> Player         -- ^ beginning player
  -> m (GameState a)
newGame r pFleet begin = do 
  (ai, eFleet) <- aiInit r
  let
    humanPlayer = PlayerState [] (generateFleet pFleet) HumanPlayer []
    aiPlayer    = PlayerState [] (generateFleet eFleet) AIPlayer []
    template    = GameState
      { currentPlayer  = undefined
      , otherPlayer    = undefined
      , aiState        = ai
      , gameRules      = r
      , expectedAction = ActionFire -- the human is expected to fire a shot
      , turnNumber     = 0
      }
    gameState = case begin of
      HumanPlayer -> template { currentPlayer = humanPlayer, otherPlayer = aiPlayer }
      AIPlayer    -> template { currentPlayer = aiPlayer, otherPlayer = humanPlayer }
  return gameState

-- | The battleship default rules
defaultRules :: Rules 
defaultRules = Rules
  { rulesShips = [ 5, 4, 4, 3, 3, 3, 2, 2, 2, 2 ]
  , rulesAgainWhenHit = True
  , rulesMove  = True
  , rulesNoviceMode = False
  , rulesDevMode = False
  , rulesMaximumTurns = 2 * 75 -- 75 turns for each player
  }

-- | Helper: Creates a grid, filled with one value.
newGrid :: (Int, Int) -> a -> Grid a
newGrid (w, h) a
  = array ((0, 0), (w - 1, h - 1))
    [((x, y), a) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

-------------------------------------------------------------------------------
-- * Helper Functions
-------------------------------------------------------------------------------

shipAdmissible :: FleetPlacement -> ShipShape -> Bool
shipAdmissible fleet ship = rangeCheck && freeCheck where
  -- check if ship is completely inside grid
  rangeCheck     = L.all (inRange gridRange)
                 $ shipCoordinates 0 ship
  -- check if ship is not overlapping the safety margin of other ships
  freeCheck      = L.all (isNothing . shipAt fleet)
                 $ shipCoordinates 1 ship
  (w, h)         = boardSize
  gridRange      = ((0, 0), (w - 1, h - 1))

-- | Calculates the position occupied by a ship including safety margin.
shipCoordinates :: HasShipShape s => Int -> s -> [Pos]
shipCoordinates margin (getShipShape -> ShipShape{..}) = 
  case shipOrientation of
    Horizontal -> [(x + i, y + d) | i <- [-margin..shipSize - 1 + margin], d <- [-margin..margin]]
    Vertical   -> [(x + d, y + i) | i <- [-margin..shipSize - 1 + margin], d <- [-margin..margin]]
  where (x, y) = shipPosition

shipAt :: (Foldable f, HasShipShape s) => f s -> Pos -> Maybe s
shipAt fleet (px, py) = find containsP fleet where
  containsP (getShipShape -> ShipShape{..}) = case shipOrientation of
    Horizontal -> px >= sx && px < sx + shipSize && py == sy
    Vertical   -> px == sx && py >= sy && py < sy + shipSize
    where (sx, sy) = shipPosition

isShipAtSunk :: Fleet -> Pos -> Bool
isShipAtSunk fleet pos = case shipAt fleet pos of
  Nothing -> False
  Just s  -> isShipSunk s

-- | Returns the zero-based index of a ship cell based on a global coordinate
shipCellIndex :: HasShipShape s => Pos -> s -> Maybe Int
shipCellIndex (px,py) (getShipShape -> ShipShape{..}) = case shipOrientation of
    Horizontal 
      | px >= sx && px < sx + shipSize && py == sy -> Just $ px - sx
    Vertical
      | px == sx && py >= sy && py < sy + shipSize -> Just $ py - sy
    _ -> Nothing
    where (sx, sy) = shipPosition

-- | Inflicts damage to the specified ship cell
damageShip :: Int -> Ship -> Ship
damageShip i ship = ship { shipDamage = shipDamage ship // [(i,True)] }

allSunk :: Fleet -> Bool
allSunk = and . fmap isShipSunk

isDamaged :: Ship -> Bool
isDamaged = or . elems . shipDamage

isShipSunk :: Ship -> Bool
isShipSunk = and . elems . shipDamage

generateFleet :: FleetPlacement -> Fleet
generateFleet = Map.fromAscList . fmap newShip . zip [1..] where
  newShip (sID, shape) = (sID, Ship sID shape (listArray (0,shipSize shape-1) (repeat False)))

shipSizes :: Rules -> [Int]
shipSizes rules = sort $ nub (rulesShips rules)

numberShipsOfSize :: [Int] -> Int -> Int
numberShipsOfSize ships size = length $ filter (== size) ships

unsunkShips :: Fleet -> [Ship]
unsunkShips fleet = filter (not . isShipSunk) (Map.elems fleet)

sizesOfShips :: [Ship] -> [Int]
sizesOfShips = map (shipSize . shipShape)

-------------------------------------------------------------------------------
-- * Turn
-------------------------------------------------------------------------------

-- | Result of the turn with respect to the current player
data Turn = Over | Again | Next deriving (Show, Eq, Ord, Enum, Bounded)

-- | Checks whether the game is drawn.
-- It is drawn when the number of shots has exceeded the
-- number rulesMaximumTurns from the rules.
isDrawn :: GameState a -> Bool
isDrawn game = curTurns >= maxTurns where
  curTurns = turnNumber game
  maxTurns = rulesMaximumTurns . gameRules $ game

-- | Performs a full AI turn. 
-- This function takes care of swapping the current player.
-- If the AI player won, the currentPlayer is not swapped back.
-- 1. according to the rules, the AI may fire once or more
-- 2. according to the rules, the AI may move one ship
aiTurn :: (MonadState (GameState a) m, MonadRandom m, AI a)
        => m Turn
aiTurn = do
    switchRoles
    result <- shots
    rules <- gets gameRules
    when (result /= Over && rulesMove rules) $ executeMove moveAI
    switchRoles
    draw <- gets isDrawn
    return $ case result of
      Next | draw -> Over
      _           -> result
  where
    shots = do
      result <- executeShot aiShot
      rules <- gets gameRules
      case result of
        -- the AI player is allowed to shoot once more, when enabled
        Again
          | rulesAgainWhenHit rules -> shots
          | otherwise               -> return Next
        -- the AI player has either won or missed
        _     -> return result

-- | This function executes the fire-part of the human's turn
-- It updates the "expectedAction" field of the state appropriately
humanTurnFire :: (MonadState (GameState a) m)
        => Pos -> m Turn
humanTurnFire target = do
  -- assert, that the human is allowed to fire a shot
  ActionFire <- gets expectedAction
  -- setup common actions
  rules <- gets gameRules
  -- fire
  result <- executeShot $ fireAt target
  case result of
    -- human has won, no further action needed
    Over -> return Over
    -- expect a move of the human player first
    -- human may fire again
    Again
      -- expected move is still "fire"
      -- otherwise: firing again not allowed, proceeding to default case
      | rulesAgainWhenHit rules -> return Again
    _ -> do
      -- update the action to "move" if appropriate
      humanFleet <- gets $ playerFleet . currentPlayer
      when (rulesMove rules && anyShipMovable rules humanFleet) $
        modify (\gs -> gs { expectedAction = ActionMove})
      return Next


-- | The current player fires at the other player
-- Modifies the list of shots fired by the player and
-- inflicts damage to the other player's ship, if hit.
fireAt :: (MonadState (GameState a) m) 
       => Pos -> m HitResponse
fireAt pos = do
  self  <- gets currentPlayer
  other <- gets otherPlayer
  let remainingFleet = Map.filter (not . isShipSunk) (playerFleet other)
  result <- case shipAt remainingFleet pos of
    Nothing -> return Water
    Just ship -> do
      let
        -- inflict damage to the ship
        Just idx = shipCellIndex pos ship
        newShip  = damageShip idx ship
        -- replace ship
        newFleet = Map.insert (shipID ship) newShip (playerFleet other)
        other'   = other { playerFleet = newFleet }
      -- update other player
      modify (\gs -> gs { otherPlayer = other' })
      return $ if isShipSunk newShip
        then Sunk
        else Hit
  -- add this shot to history
  time <- gets turnNumber
  let self' = self { playerShots = Shot pos result time : playerShots self }
  modify (\gs -> gs{currentPlayer = self'})
  return result

-- | Performs the enemies turn
-- assumes, that the enemy is the currentPlayer
aiShot :: (MonadState (GameState a) m, MonadRandom m, AI a) => m HitResponse
aiShot = do
  -- let the AI decide where to shoot
  ai <- gets aiState
  (pos, s) <- runStateT aiFire ai
  -- fire shot
  result <- fireAt pos
  -- give feedback to the AI
  (_, s')  <- runStateT (aiResponse pos result) s
  -- save modified AI state
  modify (\g -> g{aiState = s'})
  return result

-- | Executes the supplied turn.
executeShot :: (MonadState (GameState a) m)
            => (m HitResponse) -> m Turn
executeShot turn = do
  result <- turn
  op <- gets otherPlayer
  case result of
    Water -> return Next
    Hit   -> return Again
    Sunk
      | allSunk $ playerFleet op -> return Over
      | otherwise                -> return Again


-- | Switches the roles of active and passive player.
-- Every time the roles are switched, the turn number is increased.
switchRoles :: (MonadState (GameState a) m) => m ()
switchRoles = do
  modify(\g -> g
    { currentPlayer = otherPlayer g
    , otherPlayer = currentPlayer g
    })
  increaseTurnNumber

-- | increases the turn number. This function is called by `switchRoles`
-- and is not meant to be called directly.
increaseTurnNumber :: (MonadState (GameState a) m) => m ()
increaseTurnNumber = modify $ \gs -> gs {turnNumber = turnNumber gs + 1}

-------------------------------------------------------------------------------
-- * Move a ship
-------------------------------------------------------------------------------

-- | A ships movement
data Movement 
  = Forward  -- ^ -1 for the respective coordinate
  | Backward -- ^ +1 for the respective coordinate
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Tries to move the human player's ship if pos is one of its endings.
-- Assumes that the human player is the currentPlayer
moveHuman :: (MonadState (GameState a) m) 
     => Maybe Pos -> m (Maybe (ShipID, Movement))
moveHuman pos = do
  -- assert the human is really expected to move a ship
  ActionMove <- gets expectedAction
  -- change expected action
  modify (\gs -> gs { expectedAction = ActionFire } )
  -- return real move
  case pos of
    Nothing -> return Nothing
    Just p  -> desiredMove p `liftM` gets (playerFleet . currentPlayer)

-- | Lets the AI decide on a move
-- Assumes that the ai player is the currentPlayer
moveAI :: (MonadState (GameState a) m, MonadRandom m, AI a) 
     => m (Maybe (ShipID, Movement))
moveAI = do
  ai <- gets aiState
  fleet <- gets (playerFleet . currentPlayer)
  shots <- gets (playerShots . otherPlayer)
  (mov, s) <- runStateT (aiMove fleet shots) ai
  modify (\g -> g{aiState = s})
  return mov

-- | executes a move for the current player
executeMove :: (MonadState (GameState a) m, MonadRandom m, AI a) 
     => m (Maybe (ShipID, Movement)) -> m ()
executeMove moveAction = do
  move <- moveAction
  curPlayer <- gets currentPlayer
  let fleet = playerFleet curPlayer
  case move of
    Nothing               -> return ()
    Just (shipID, movement) -> case Map.lookup shipID fleet of
      Just ship -> when (not $ isDamaged ship) $ do
        time <- gets turnNumber
        let
          newFleet   = moveShip ship movement fleet
          curPlayer' = curPlayer 
            { playerFleet = newFleet
            , playerMoves = ShipMove shipID movement time : playerMoves curPlayer 
            }
        modify (\gs -> gs { currentPlayer = curPlayer' })
      Nothing -> return ()

-- | Find out which ship the player wants to move into which direction.
desiredMove :: Pos -> Fleet -> Maybe (ShipID, Movement)
desiredMove pos fleet = do 
  Ship{..} <- shipAt remainingFleet pos
  let 
    (x,y) = shipPosition $ shipShape
    size  = shipSize shipShape
  case shipOrientation shipShape of
    Horizontal 
      | pos == (x, y)            -> Just (shipID, Forward)
      | pos == (x + size - 1, y) -> Just (shipID, Backward)
    Vertical
      | pos == (x, y)            -> Just (shipID, Forward)
      | pos == (x, y + size - 1) -> Just (shipID, Backward)
    _ -> Nothing
  where remainingFleet = Map.filter (not . isDamaged) fleet

-- | Only moves the ship if it complies with the given rules.
moveShip :: Ship -> Movement -> Fleet -> Fleet
moveShip ship movement fleet = 
  if isMovable movement fleet ship
    then Map.adjust (\s -> s{shipShape = newShape}) (shipID ship) fleet
    else fleet
  where newShape = movedShipShape movement (shipShape ship)
-- | Checks whether a ship can be moved.
isMovable :: Movement -> Fleet -> Ship -> Bool
isMovable movement fleet ship =
       shipAdmissible otherShips newShape
    && not (isDamaged ship) 
  where
    newShape = movedShipShape movement (shipShape ship)
    otherShips = map (shipShape . snd)
               . Map.toAscList
               . Map.filter (not . isShipSunk) -- this ship can move over sunk ships
               . Map.delete (shipID ship)
               $ fleet

-- | Returns whether any ship in the given fleet is movable.
anyShipMovable :: Rules -> Fleet -> Bool
anyShipMovable rules fleet = rulesMove rules 
    && (anyForward || anyBackward)
  where
    anyForward  = or $ fmap (isMovable Forward fleet) fleet
    anyBackward = or $ fmap (isMovable Backward fleet) fleet

-- | Ship after movement was made.
movedShipShape :: Movement -> ShipShape -> ShipShape
movedShipShape movement ship = case (shipOrientation ship, movement) of
  (Horizontal, Forward)  -> ship {shipPosition = (x - 1, y)}
  (Horizontal, Backward) -> ship {shipPosition = (x + 1, y)}
  (Vertical, Forward)    -> ship {shipPosition = (x, y - 1)}
  (Vertical, Backward)   -> ship {shipPosition = (x, y + 1)}
  where (x,y) = shipPosition ship

-------------------------------------------------------------------------------
-- * Path Pieces
-------------------------------------------------------------------------------

instance PathPiece Rules where
  fromPathPiece = impBinary >=> eitherToMaybe . decode . toStrict
  toPathPiece   = expBinary . fromStrict . encode

newtype Ships = Ships [Int] deriving (Eq, Read, Show)

instance Serialize Ships where
  get = Ships <$> getList8 getIntegral8
  put (Ships ships) = putList8 putIntegral8 ships

instance PathPiece Ships where
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
  get = GameState <$> get <*> get <*> get <*> get <*> get <*> getIntegral8
  put GameState {..} = do
    put currentPlayer
    put otherPlayer
    put aiState
    put gameRules
    put expectedAction
    putIntegral8 turnNumber

instance Serialize PlayerState where
  get = PlayerState <$> getList8 get <*> get <*> get <*> get
  put PlayerState{..} = do
    putList8 put playerShots
    put playerFleet
    put playerType
    put playerMoves

instance Serialize Rules where
  get = Rules <$> getList8 getIntegral8 <*> get <*> get <*> get <*> get <*> getIntegral8
  put Rules {..} = do
    putList8 putIntegral8 rulesShips
    put rulesAgainWhenHit
    put rulesMove
    put rulesNoviceMode
    put rulesDevMode
    putIntegral8 rulesMaximumTurns

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

putPos :: Putter Pos
putPos (x,y) = put (hi .|. lo :: Word8) where
  hi = (fromIntegral x .&. 0x0F) `shiftL` 4
  lo =  fromIntegral y .&. 0x0F

getPos :: Get Pos
getPos = fromB <$> getWord8 where
  fromB b = ( fromIntegral $ b `shiftR` 4
            , fromIntegral $ b .&. 0x0F   )

putEnum8 :: Enum a => Putter a
putEnum8 = putWord8 . fromIntegral . fromEnum

getEnum8 :: Enum a => Get a
getEnum8 = toEnum . fromIntegral <$> getWord8

putIntegral8 :: Integral a => Putter a
putIntegral8 = putWord8 . fromIntegral

getIntegral8 :: Integral a => Get a
getIntegral8 = fromIntegral <$> getWord8

putList8 :: Putter a -> Putter [a]
putList8 pel xs = do
  putIntegral8 $ length xs
  forM_ xs pel

getList8 :: Get a -> Get [a]
getList8 gel = getIntegral8 >>= getList where
  getList len = replicateM len gel

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
-------------------------------------------------------------------------------
-- * Random
-------------------------------------------------------------------------------

instance Random Orientation where
  randomR (a, b) g = (toEnum r, g') where
    (r, g')        = randomR (fromEnum a, fromEnum b) g
  random g         = randomR (Horizontal, Vertical) g

instance Random Movement where
  randomR (a, b) g = (toEnum r, g') where
    (r, g')        = randomR (fromEnum a, fromEnum b) g
  random g         = randomR (Forward, Backward) g

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
  parseJSON (Number (I i)) = return $ toEnum $ fromIntegral i
  parseJSON (Number (D d)) = return $ toEnum $ floor d
  parseJSON _              = mzero

instance ToJSON Orientation where
  toJSON = Number . I . fromIntegral . fromEnum

instance ToJSON ShipShape where
  toJSON (ShipShape {..}) = object 
    [ "X"           .= fst shipPosition
    , "Y"           .= snd shipPosition
    , "Size"        .= shipSize
    , "Orientation" .= shipOrientation
    ]
