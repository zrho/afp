----------------------------------------------------------------------------
-- |
-- Module      :  Logic.Game
-- Stability   :  experimental
-- Portability :  semi-portable
--
-- Logic and data structures of the battleships implementation.
--
-- The AI is implemented in another module to cleanly seperate it from
-- the game logic.

{-# LANGUAGE ViewPatterns #-}
module Logic.Game
  ( 
  -- * Game Functions
    boardSize
  , gridRange
  , fleetShips
  , newGame
  , humanPlayerState
  , aiPlayerState
  -- * Turn Functions
  , isDrawn
  , isTimedOut
  , numRemainingShips
  , remainingTurns
  , isCountdownStart
  , aiTurn
  , desiredMove
  , executeMove
  , executeShot
  , fireAt
  , switchRoles
  , humanTurnFire
  , moveHuman
  -- * Ship Functions
  , allSunk 
  , damageShip
  , generateFleet
  , isDamaged
  , isMovable
  , isShipSunk
  , shipsAt
  , shipSinkTime
  , sinkTime
  , moveShip
  , uncheckedMoveShip
  , numberShipsOfSize
  , shipAdmissible
  , shipCellIndex
  , shipCoordinates
  , sizesOfShips
  , unsunkShips
  ) where

import           Prelude hiding (and, or, foldl, foldr, mapM_)
import           Control.Monad hiding (forM_, mapM_)
import           Control.Monad.Random
import           Control.Monad.Trans.State (runStateT)
import           Control.Monad.State.Class (MonadState, gets, modify)
import           Data.Array
import           Data.Foldable
import           Data.List as L hiding (and, or, foldl, foldr, find)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Logic.Types
import           Logic.Util

-------------------------------------------------------------------------------
-- * Constants
-------------------------------------------------------------------------------

boardSize :: (Int, Int)
boardSize = (10, 10)

-- | A sorted list of available ships
fleetShips :: [Int]
fleetShips = sort [ 2, 2, 2, 2, 3, 3, 3, 4, 4, 5 ]

-------------------------------------------------------------------------------
-- * New Games
-------------------------------------------------------------------------------

-- | Creates a new game.
newGame
  :: (AI a, MonadRandom m)
  => Rules          -- ^ rules of the game
  -> Bool           -- ^ novice mode?
  -> Bool           -- ^ developer mode?
  -> FleetPlacement -- ^ fleet of the human player
  -> Player         -- ^ beginning player
  -> m (GameState a)
newGame r noviceMode devMode pFleet begin = do 
  (ai, eFleet) <- aiInit r
  let
    humanPlayer = PlayerState [] (generateFleet pFleet) HumanPlayer []
    aiPlayer    = PlayerState [] (generateFleet eFleet) AIPlayer []
    template    = GameState
      { currentPlayer  = undefined
      , otherPlayer    = undefined
      , aiState        = ai
      , gameRules      = r
      , noviceModeOpt  = noviceMode
      , devModeOpt     = devMode
      , expectedAction = ActionFire -- the human is expected to fire a shot
      , turnNumber     = 0
      }
    gameState = case begin of
      HumanPlayer -> template { currentPlayer = humanPlayer, otherPlayer = aiPlayer }
      AIPlayer    -> template { currentPlayer = aiPlayer, otherPlayer = humanPlayer }
  return gameState

-------------------------------------------------------------------------------
-- * Helper Functions
-------------------------------------------------------------------------------

gridRange :: (Pos, Pos)
gridRange = ((0, 0), (w - 1, h - 1))
  where (w, h) = boardSize

shipAdmissible :: FleetPlacement -> ShipShape -> Bool
shipAdmissible fleet ship = rangeCheck && freeCheck where
  -- check if ship is completely inside grid
  rangeCheck     = L.all (inRange gridRange)
                 $ shipCoordinates 0 ship
  -- check if ship is not overlapping the safety margin of other ships
  freeCheck      = L.all (null . shipsAt fleet)
                 $ shipCoordinates 1 ship

-- | Calculates the position occupied by a ship including safety margin.
shipCoordinates :: HasShipShape s => Int -> s -> [Pos]
shipCoordinates margin (getShipShape -> ShipShape{..}) = 
  case shipOrientation of
    Horizontal -> [(x + i, y + d) | i <- [-margin..shipSize - 1 + margin], d <- [-margin..margin]]
    Vertical   -> [(x + d, y + i) | i <- [-margin..shipSize - 1 + margin], d <- [-margin..margin]]
  where (x, y) = shipPosition

-- | Returns the ships that contain the given positon.
shipsAt :: (Foldable f, HasShipShape s) => f s -> Pos -> [s]
shipsAt fleet (px, py) = filter containsP $ toList fleet where
  containsP (getShipShape -> ShipShape{..}) = case shipOrientation of
    Horizontal -> px >= sx && px < sx + shipSize && py == sy
    Vertical   -> px == sx && py >= sy && py < sy + shipSize
    where (sx, sy) = shipPosition

-- | Determine the time when a given ship was sunk.
shipSinkTime :: Ship -> Time
shipSinkTime ship = if isShipSunk ship then lastHitTime ship else mempty

-- | Determine the time when a given ship was sunk.
lastHitTime :: Ship -> Time
lastHitTime = fold . shipDamage -- let the maximum monoid take care of it

-- | If at the given position there is a completely sunk ship, this stores the turn number in which it was sunk.
sinkTime :: Fleet -> Array Pos Time
sinkTime fleet = buildArray gridRange $ \pos ->
  let isPosInShip = isJust . shipCellIndex pos
  in foldMap shipSinkTime $ Map.filter isPosInShip fleet

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
damageShip :: Int -> Int -> Ship -> Ship
damageShip time i ship = ship { shipDamage = shipDamage ship // [(i, Time $ Just time)] }

allSunk :: Fleet -> Bool
allSunk = and . fmap isShipSunk

-- | Calculates how many ships of a given fleet are not sunk yet
numRemainingShips :: Fleet -> Int
numRemainingShips = Map.size . Map.filter (not . isShipSunk)

isDamaged :: Ship -> Bool
isDamaged = L.any (isJust . unwrapTime) . elems . shipDamage

isShipSunk :: Ship -> Bool
isShipSunk = L.all (isJust . unwrapTime) . elems . shipDamage

generateFleet :: FleetPlacement -> Fleet
generateFleet = Map.fromAscList . fmap newShip . zip [1..] where
  newShip (sID, shape) = (sID, Ship sID shape (listArray (0,shipSize shape-1) (repeat $ Time Nothing)))

numberShipsOfSize :: [Int] -> Int -> Int
numberShipsOfSize ships size = length $ filter (== size) ships

unsunkShips :: Fleet -> [Ship]
unsunkShips fleet = filter (not . isShipSunk) (Map.elems fleet)

sizesOfShips :: [Ship] -> [Int]
sizesOfShips = map (shipSize . shipShape)

humanPlayerState :: GameState a -> PlayerState
humanPlayerState GameState{..} = case playerType currentPlayer of
  HumanPlayer -> currentPlayer
  AIPlayer    -> otherPlayer

aiPlayerState    :: GameState a -> PlayerState
aiPlayerState GameState{..} = case playerType currentPlayer of
  HumanPlayer -> otherPlayer
  AIPlayer    -> currentPlayer

-------------------------------------------------------------------------------
-- * Turn Common
-------------------------------------------------------------------------------

-- | Checks whether the game is drawn.
-- It is drawn when it has timed out and both players 
-- have sunk the same number of ships.
isDrawn :: GameState a -> Bool
isDrawn game = isTimedOut game && remA == remB where
  remA = numRemainingShips $ playerFleet $ currentPlayer game
  remB = numRemainingShips $ playerFleet $ otherPlayer game

-- | Checks whether the game has timed out.
-- It is timed out when the number of shots has exceeded the
-- number rulesMaximumTurns from the rules.
isTimedOut :: GameState a -> Bool
isTimedOut game = curTurns >= maxTurns where
  curTurns = turnNumber game
  maxTurns = rulesMaximumTurns . gameRules $ game

-- | Calculates the number of remaining turns overall.
remainingTurns :: GameState a -> Int
remainingTurns game = maxTurns - curTurns where
  curTurns = turnNumber game
  maxTurns = rulesMaximumTurns . gameRules $ game

-- | Determines whether the countdown starts right now.
-- It starts when exactly countdownTurns turns remain.
isCountdownStart :: GameState a -> Bool
isCountdownStart game = remTurns == cdTurns where
  remTurns = remainingTurns game
  cdTurns = rulesCountdownTurns . gameRules $ game

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

-- | The current player fires at the other player
-- Modifies the list of shots fired by the player and
-- inflicts damage to the other player's ship, if hit.
fireAt :: (MonadState (GameState a) m) 
       => Pos -> m HitResponse
fireAt pos = do
  self  <- gets currentPlayer
  other <- gets otherPlayer
  time <- gets turnNumber
  let remainingFleet = Map.filter (not . isShipSunk) (playerFleet other)
  result <- case shipsAt remainingFleet pos of
    [] -> return Water
    [ship] -> do
      let
        -- inflict damage to the ship
        Just idx = shipCellIndex pos ship
        newShip  = damageShip time idx ship
        -- replace ship
        newFleet = Map.insert (shipID ship) newShip (playerFleet other)
        other'   = other { playerFleet = newFleet }
      -- update other player
      modify (\gs -> gs { otherPlayer = other' })
      return $ if isShipSunk newShip
        then Sunk
        else Hit
    _ -> error $ "Multiple unsunk ships at position " ++ show pos ++ ". This shouldn't happen!"
  -- add this shot to history
  let self' = self { playerShots = Shot pos result time : playerShots self }
  modify (\gs -> gs{currentPlayer = self'})
  return result

-- | Executes the supplied turn.
executeShot :: (MonadState (GameState a) m)
            => HitResponse -> m Turn
executeShot result = do
  rules <- gets gameRules
  op <- gets otherPlayer
  handleDraw $ case result of
    Water -> Next
    Hit
      | rulesAgainWhenHit rules -> Again
      | otherwise               -> Next
    Sunk
      | allSunk $ playerFleet op -> Over
      | rulesAgainWhenHit rules  -> Again
      | otherwise                -> Next
  where
    handleDraw r = do
      timedOut <- gets isTimedOut
      return $ case r of
        Next | timedOut -> Over
        _               -> r

-------------------------------------------------------------------------------
-- * Turn AI
-------------------------------------------------------------------------------

-- | Performs a full AI turn. 
-- This function takes care of swapping the current player.
-- 1. according to the rules, the AI may fire once or more
-- 2. according to the rules, the AI may move one ship
aiTurn :: (MonadState (GameState a) m, MonadRandom m, AI a)
        => m Turn
aiTurn = do
    switchRoles
    result <- shots
    rules <- gets gameRules
    when (result /= Over && rulesMove rules) $ moveAI >>= executeMove
    switchRoles
    return result
  where
    shots = do
      result <- aiShot >>= executeShot
      case result of
        -- the AI player is allowed to shoot once more, when enabled
        Again -> shots
        _     -> return result

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



-------------------------------------------------------------------------------
-- * Turn Human
-------------------------------------------------------------------------------

-- | This function executes the fire-part of the human's turn
-- It updates the "expectedAction" field of the state appropriately
humanTurnFire :: (MonadState (GameState a) m)
        => Pos -> m Turn
humanTurnFire target = do
  -- assert that the human is allowed to fire a shot
  ActionFire <- gets expectedAction
  -- setup common actions
  rules <- gets gameRules
  -- fire
  result <- fireAt target >>= executeShot
  when (result == Next) $ do
      -- update the action to "move" if appropriate
      humanFleet <- gets $ playerFleet . currentPlayer
      when (rulesMove rules && anyShipMovable rules humanFleet) $
        modify (\gs -> gs { expectedAction = ActionMove})
  return result

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

-- | Find out which ship the player wants to move into which direction.
desiredMove :: Pos -> Fleet -> Maybe (ShipID, Movement)
desiredMove pos fleet = do 
  Ship{..} <- case shipsAt remainingFleet pos of
    []     -> Nothing
    [ship] -> Just ship
    _      -> error $ "Multiple unsunk ships at positon " ++ show pos ++ ". This shouldn't happen!"
  let 
    (x,y) = shipPosition shipShape
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

-------------------------------------------------------------------------------
-- * Moving Ships
-------------------------------------------------------------------------------

-- | executes a move for the current player
executeMove :: (MonadState (GameState a) m)
     => Maybe (ShipID, Movement) -> m ()
executeMove move = do
  curPlayer <- gets currentPlayer
  let fleet = playerFleet curPlayer
  case move of
    Nothing                 -> return ()
    Just (shipID, movement) -> case Map.lookup shipID fleet of
      Just ship -> unless (isDamaged ship) $ do
        time <- gets turnNumber
        let
          newFleet   = moveShip ship movement fleet
          curPlayer' = curPlayer 
            { playerFleet = newFleet
            , playerMoves = ShipMove shipID movement time : playerMoves curPlayer 
            }
        modify (\gs -> gs { currentPlayer = curPlayer' })
      Nothing -> return ()

-- | Only moves the ship if it complies with the given rules.
moveShip :: Ship -> Movement -> Fleet -> Fleet
moveShip ship movement fleet = 
  if isMovable movement fleet ship
    then uncheckedMoveShip ship movement fleet
    else fleet

-- | Like moveShip but without movability check; needed for replay.
uncheckedMoveShip :: Ship -> Movement -> Fleet -> Fleet
uncheckedMoveShip ship movement = 
  Map.adjust (\s -> s{shipShape = newShape}) (shipID ship)
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
