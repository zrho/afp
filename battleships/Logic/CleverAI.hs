----------------------------------------------------------------------------
-- |
-- Module      :  Logic.Binary
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Clever AI for battleships that supports moving ships.
--
-- Firing shots:
-- 
-- Blocked cells:
-- Each square is assigned a probability to be blocked (i.e. being part of no ship).
-- For example if we just hit a water cell, we know it's blocked. (Probablity 1)
-- But after some time a ship can move there, so this probability declines over time.
-- (modeled as exponential decay.)
-- 
-- Immovable ships:
-- For each position, we count how many ships this square can be part of. We only
-- consider ships which are not ruled out by to the AI's information about the
-- situation. Hit ships are weighted higher => We try to sink ships as fast as
-- possible. (This is not the case for movable ships!)
-- 
-- Movable ships:
-- There are 2 phases. In the first one, the AI follows a checkerboard pattern to
-- hit (not sink!) as many ships as possible to immobilize them.
-- 
-- After a certain time has passed or enough ships have been hit, we move on to
-- phase 2.
-- 
-- Now that we have (hopefully) hit all ships, we sink them (essentially) according
-- to the strategy for immovable ships.
-- (However, a checkerboard pattern isn't useful now.)
--
-- Both movable and immovable ships:
-- Positions at the edge of the board naturally allow fewer ships to pass through
-- them. But the AI shouldn't be a bias towards the center, so the scores are divided
-- by the scores at the beginning of the game.
-- 
-- Finally: Some randomness is added to the scores. The amount of randomness depends
-- on the selected difficulty level.
-- The highest scoring position is then chosen.

module Logic.CleverAI 
  ( CleverAI
  ) where

import           Prelude
import           Data.Array
import           Data.List ((\\), elemIndex, intersect, delete)
import           Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as Map
import qualified Data.Foldable as Foldable
import           Logic.Game
import           Logic.AIUtil
import           Logic.Random
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Applicative
import           Data.Serialize (Serialize)
import qualified Data.Serialize as S

data CleverAI = CleverAI
  { rules            :: Rules        -- ^ rules of this game
  , tracking         :: TrackingGrid -- ^ stores what was hit the last time at each position
  , shots            :: [Pos]        -- ^ AI's previous shots
  , sunk             :: [ShipShape]  -- ^ ships of the user's fleet that are already sunk
  , sunkTime         :: [Int]        -- ^ what was the number of shots fired when the respective ship was sunk
  , checkerboardEven :: Bool
  }

-- | Constructs an initial AI instance.
cleverAI :: Rules -> Bool -> CleverAI
cleverAI r checkerboardEven = CleverAI
  { rules            = r
  , tracking         = newGrid boardSize Nothing
  , shots            = []
  , sunk             = []
  , sunkTime         = []
  , checkerboardEven = checkerboardEven
  }

instance AI CleverAI where
  aiInit r = do
    fleet <- liftM fromJust $ initShips rulesShips []
    checkerboardEven <- getRandom
    return (cleverAI r checkerboardEven, fleet)
  aiFire         = liftM maximumIx $ scoreGrid >>= randomize
  aiResponse p r = modify (cleverResponse p r)
  aiMove fleet _ = do
    move <- chooseRandom $
      [ (shipID ship, mvmt)
      | ship <- Map.elems fleet
      , mvmt <- [Forward, Backward]
      , isMovable mvmt fleet ship
      ]
    rand <- getRandomR (0.0, 1.0)
    return $ if rand < probMove then move else Nothing

-- | How often should the AI use its right to move?
probMove :: Double
probMove = 0.5

cleverResponse :: Pos -> HitResponse -> CleverAI -> CleverAI
cleverResponse p r ai = case r of
  Sunk -> ai'
    { sunk     = sunkShip           : sunk ai'      -- add sunk ship
    , sunkTime = length (shots ai') : sunkTime ai'  -- add time for sinking
      -- remove this ship from the tracking; otherwise it will confuse other functions:
    , tracking = tracking ai' // [(pos, Nothing) | pos <- shipArea sunkShip (tracking ai')]
    } where
      sunkShip = fromMaybe err $ findSunkShip (tracking ai') p
      err      = error $ "No sunk ship found: " ++ show p ++ "\n" ++ showTracking (tracking ai')
  _    -> ai'
  where
    shipArea s t = shipCoordinates 1 s `intersect` indices t
    ai' = ai
      { tracking = tracking ai // [(p, Just r)]
      , shots    = p : shots ai
      }    

--------------------------------------------------------------------------------
-- * Firing shots
--------------------------------------------------------------------------------

-- | Add some randomness to the scores: Multiplies each score with a
-- | random number near 1. If the AI is configured to play not as strongly,
-- | the range of the random numbers is increased.
randomize :: (MonadRandom m, MonadState CleverAI m) => ScoreGrid -> m ScoreGrid
randomize s = do
  d <- gets $ rulesDifficulty . rules
  move <- gets $ rulesMove . rules
  let m = Foldable.maximum s
  flip traverseArray s $ \r -> case d of
    Hard   -> liftM (r *) $ getRandomR (0.95,1.05)
    Medium -> if move
      then liftM (r +) $ getRandomR (0, m * 2)   -- chosen s.t. AI needs about 10 more shots
      else liftM (r +) $ getRandomR (0, m * 2.9) -- on average (using aibenchmark)
    Easy   -> liftM (r +) $ getRandomR (0, m * 3.5) -- chosen s.t. AI needs about 20 more shots on average

-- | Assigns each cell a score. If it's high, it means that it's beneficial
-- | to attack this cell. On how this is calculated, see below.

scoreGrid :: (MonadState CleverAI m) => m ScoreGrid
scoreGrid = scoreGrid' `liftM` get

scoreGrid' :: CleverAI -> ScoreGrid
scoreGrid' ai@(CleverAI {..}) = buildArray bs $ scorePosition ai remaining where
  (w, h)    = boardSize
  bs        = ((0, 0), (w - 1, h - 1))
  remaining = rulesShips \\ map shipSize sunk

-- | Given a position, look in all directions to find out whether it's part of a sunk ship.
findSunkShip :: TrackingGrid    -- ^ AI's tracking array
             -> Pos             -- ^ cell where the ship was sunk
             -> Maybe ShipShape -- ^ sunk ship, if one exists
findSunkShip t p = findHorizontal `mplus` findVertical where
  findHorizontal =
    let left@(leftX,_) = findEnd (\(x,y) -> (x-1,y)) p
        (rightX,_)     = findEnd (\(x,y) -> (x+1,y)) p
        len            = rightX - leftX + 1
    in if len < 2 then Nothing else Just $ ShipShape left len Horizontal
  findVertical =
    let up@(_,upY) = findEnd (\(x,y) -> (x,y-1)) p
        (_,downY)  = findEnd (\(x,y) -> (x,y+1)) p
        len        = downY - upY + 1
    in if len < 2 then Nothing else Just $ ShipShape up len Vertical
  -- | Move as long as cells are hit or sunk, until the end of the ship is reached.
  findEnd :: (Pos -> Pos) -- ^ the movement
          -> Pos          -- ^ start position
          -> Pos
  findEnd move pos = if inRange (bounds t) (move pos) && isHitOrSunk (t ! move pos)
                     then findEnd move $ move pos
                     else pos


-- | Given a position pos, assign it a score. How this is calculated
-- | depends on whether the ships are movable or not. (See comment above.)
scorePosition :: CleverAI
              -> [Int]          -- ^ lengths of the remaining ships
              -> Pos            -- ^ position to be scored
              -> Score
scorePosition ai@(CleverAI {..}) remaining pos@(x,y) =
  if rulesMove rules then scoreMovable else scoreImmovable where

  -- | Score position when ships are immovable.
  scoreImmovable :: Score
  scoreImmovable = preventDoubleAttackImmovable
                 . considerEdges
                 . checkerboard 0.9
                 . scoreShips
                 $ allRemaining

  -- | Score position when ships are movable.
  scoreMovable :: Score
  scoreMovable = if hitCellsCount < minRequiredHits
    -- This condition determines when we have hit enough ships to move on to the second phase.
    then phase1 else phase2 where
    -- | Minimum number of hits required to move on to the 2nd phase.
    -- | When a checkerboard pattern is applied, this is the minimum number of
    -- | hits if all "white squares" of all ships are hit.
    -- | For then, we want all ships to be hit at least once.
    minRequiredHits = sum
                    . map (`div` 2) -- we can hit half of the ships cells (checkerboard)
                    $ remaining
    hitCellsCount   = length
                    . filter isHit
                    . elems
                    $ tracking
    phase1          = preventDoubleAttackMovable
                    . considerEdges
                    . checkerboard 0
                    $ sum (map scoreShipPhase1 allRemaining)
    phase2          = preventDoubleAttackMovable
                    -- considerEdges doesn't work very well here
                    . scoreShips
                    $ allRemaining
  
  allRemaining :: [ShipShape]
  allRemaining = allShips pos remaining

  -- | Special scoring function for phase 1 with movable ships.
  -- | A different one is needed because we don't want to sink ships
  -- | during that phase.
  scoreShipPhase1 :: ShipShape -> Score
  scoreShipPhase1 ship = if   any (isHit . (tracking !)) $ shipCoordinates 0 ship
                         then 0 -- we don't want to sink ships yet! -> low score
                         else probNotBlocked ship

  -- | Score low if we recently fired at this position.
  -- | It is unlikely to have changed.
  preventDoubleAttackMovable :: Score -> Score
  preventDoubleAttackMovable s = if isHitOrSunk $ tracking ! pos then 0 else factor * s where
    factor = maybe 1 (\i -> 1 - decay ^ i) $ elemIndex pos shots

  -- | Score 0 if we ever before fired at this position.
  preventDoubleAttackImmovable :: Score -> Score
  preventDoubleAttackImmovable = if isHitOrSunk $ tracking ! pos then const 0 else id

  checkerboard :: Double -- ^ how should "black" fields be weighted? (between 0 and 1)
               -> Score
               -> Score
  checkerboard p 
    | checkerboardEven = if even (x + y) then id else (*) p
    | otherwise        = if odd (x + y) then id else (*) p

  -- | Divide the scores by the initial score of that position.
  -- This way, edge positions won't get a low score simply because
  -- they are at the edge. We're measuring the "difference" between
  -- the initial scores and the current scores instead.
  considerEdges :: Score -> Score
  considerEdges = (/ initialScore) where
    initialScore = fromIntegral . length $ allShips pos rulesShips

  -- | Assign a score to a list of ships. Hit ships are scored higher.
  scoreShips :: [ShipShape] -> Score
  scoreShips ships = sum (map probNotBlocked ships)
                   + 20 * scoreHit ships
                   + 50 * scoreSunk ships

  -- | Score ships that are hit.
  scoreHit :: [ShipShape] -> Score
  scoreHit ships = sum
                 . map (\s -> probNotBlocked s * rewardMultipleHits s)
                 $ filter shipHit ships where
    rewardMultipleHits s = fromIntegral
                         . length
                         . filter (isHit . (tracking !))
                         $ shipCoordinates 0 s

  -- | Score ships that are sunk.
  scoreSunk :: [ShipShape] -> Score
  scoreSunk ships = sum
                  . map probNotBlocked
                  $ filter shipSunk ships

  shipHit :: ShipShape -> Bool
  shipHit s = any (isHit . (tracking !)) $ shipCoordinates 0 s

  shipSunk :: ShipShape -> Bool
  shipSunk s = all (isHit . (tracking !)) . delete pos $ shipCoordinates 0 s

  probBlocked :: ScoreGrid
  probBlocked = probBlockedGrid ai

  -- | Compute the probability for the given ship to exist.
  probNotBlocked :: ShipShape -> Score
  probNotBlocked = product
                 . map ((1 -) . (probBlocked !))
                 . shipCoordinates 0

-- | Generates all ships through the current position which are inside the
-- | boundaries of the field.
allShips :: Pos   -- ^ the position that all ships will contain
         -> [Int] -- ^ the lengths of the ships to be generated
         -> [ShipShape]
allShips (x,y) lens = filter (shipAdmissible [])
              $ lens >>= \len ->
                  [ShipShape (x-dx, y) len Horizontal | dx <- [0..len-1]]
                  ++ [ShipShape (x, y-dy) len Vertical | dy <- [0..len-1]]

-- | Returns a grid where the value at each position is the estimated probability
-- | that this position is blocked, i.e. doesn't contain a ship.
-- | For example, when a ship was recently sunk, it's unlikely that there will be a ship at that spot.
-- | Or say, few attacks ago, we hit water, then this will stay the same for some time.
-- | Put the probablity for that will decline over time. (E.g. a ship can move on a (former) water cell.)
-- | It is modeled as exponential decay.
probBlockedGrid :: CleverAI -> ScoreGrid
probBlockedGrid (CleverAI {..}) = array ((0, 0), (width - 1, height - 1))
  [(pos, probBlocked pos)
  | x <- [0..width-1]
  , y <-[0..height-1]
  , let pos = (x,y)] where
    (width, height)     = boardSize
    probBlocked p       = maximum [probWater p, probNearSunk p,  diagonalHit p]
    numMovesAgo p       = fromMaybe (error $ "numMovesAgo with invalid position " ++ show p
                                           ++ ", shots " ++ show shots)
                          $ p `elemIndex` shots
    decayFactor         = if rulesMove rules then decay else 1 :: Score
    -- | Probability for a ship to be on a (former) water cell
    probWater p         = if isWater $ tracking ! p
                          then decayFactor ^ numMovesAgo p -- exponential "decay"
                          else 0
    -- | Probability for a ship to be within the safety zone of a sunk ship.
    probNearSunk p      = maximum'
                        . map (\(_,c) -> decayFactor ^ (length shots - c))
                        . filter (\(s,_) -> p `elem` shipCoordinates 1 s)
                        $ zip sunk sunkTime where
    -- | If the diagonal cells are hit, there can't be a ship.
    -- | Illustration:
    -- | ???
    -- | ?H?
    -- | ???
    -- | We know that the positions marked with X are blocked for *sure*:
    -- | X?X
    -- | ?H?
    -- | X?X
    -- | so diagonalHit returns 1
    diagonalHit         = fromBool
                        . any isHitOrSunk
                        . map (tracking !)
                        . diagonalCells
    diagonalCells (x,y) = [ (x-1,y-1)
                          , (x-1,y+1)
                          , (x+1,y-1)
                          , (x+1,y+1)
                          ] `intersect`
                          indices tracking

-- | How quickly should the probability for a former water cell
-- | decline? 0.99 is pretty slowly, 0.9 is very (too) quickly.
decay :: Score
decay = 0.98

-------------------------------------------------------------------------------
-- * Serialization
-------------------------------------------------------------------------------

instance Serialize CleverAI where
  get = CleverAI <$> S.get
                 <*> getSmallGrid S.get
                 <*> getList8 getPos
                 <*> getList8 S.get
                 <*> getList8 getIntegral8
                 <*> S.get
  put CleverAI {..} = do
    S.put rules
    putSmallGrid S.put tracking
    putList8 putPos shots
    putList8 S.put sunk
    putList8 putIntegral8 sunkTime
    S.put checkerboardEven
