{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Logic.CleverAI (CleverAI) where

import           Prelude
import           Data.Array
import           Data.List ((\\), elemIndex, intersect)
import           Data.Word8
import qualified Data.Map as Map
import           Logic.Game
import           Logic.AIUtil
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Applicative
import           Data.Serialize (Serialize)
import qualified Data.Serialize as S

data CleverAI = CleverAI { rules    :: Rules          -- ^ rules of this game
                         , tracking :: TrackingGrid   -- ^ stores what was hit the last time at each position
                         , shots    :: [Pos]          -- ^ AI's previous shots
                         , sunk     :: [ShipShape]    -- ^ ships of the user's fleet that are already sunk
                         , sunkTime :: [Int]          -- ^ what was the number of shots fired when the respective ship was sunk
                         }

instance AI CleverAI where

  aiInit r       = liftM2 (,)
                     (return CleverAI { rules    = r
                                      , tracking = newGrid (rulesSize r) Nothing
                                      , shots    = []
                                      , sunk     = []
                                      , sunkTime = []
                                      }
                     )
                     (initShips r)

  aiFire         = liftM chooseMaximum $ randomize =<< scoreGrid  where
    chooseMaximum :: ScoreGrid -> Pos
    chooseMaximum arr = foldl1 (\maxP p -> if arr ! p > arr ! maxP then p else maxP) (indices arr)


  aiResponse p r = modify update where
    update ai =
      let tracking' = (tracking ai) // [(p, Just r)]
          shots'    = p:(shots ai)
          (sunk', sunkTime', tracking'') = case r of
            Sunk -> let mSunkShip = findSunkShip tracking' p
                        sunkShip  = case mSunkShip of
                          Nothing -> error $ "No sunk ship found: " ++ show p ++ "\n" ++ showTracking tracking'
                          Just x  -> x in
                    ( sunkShip:sunk ai -- add sunk ship
                    , (length shots'):sunkTime ai -- add time for sinking
                    -- remove this ship from the tracking; otherwise it will irritate other functions:
                    , tracking' // [(pos, Nothing) | pos <- shipArea sunkShip tracking']
                    )
            _    -> (sunk ai, sunkTime ai, tracking')
      in ai { tracking =  {- trace' (\t -> "Tracking:\n" ++ showTracking t) -} tracking''
            , shots    = shots'
            , sunk     =  {- trace' (\f -> "AI sunk:\n" ++ showFleetPlacement (rules ai) f) -} sunk'
            , sunkTime = sunkTime' } where
      shipArea s t = shipCoordinates (rulesSafetyMargin (rules ai)) s `intersect` indices t

  aiMove fleet = gets rules >>= \rls -> chooseRandom $ generateMoves rls fleet where
    chooseRandom :: MonadRandom m => [Maybe a] -> m (Maybe a)
    chooseRandom [] = return Nothing
    chooseRandom xs = (xs !!) `liftM` getRandomR (0, length xs - 1)
    generateMoves :: Rules -> Fleet -> [Maybe (ShipID, Movement)]
    generateMoves r f = map (\(s, m) -> Just (shipID s, m))
                      . filter (isMovable r f)
                      $ liftM2 (,) (Map.elems f) [Forward, Backward]
    isMovable :: Rules -> Fleet -> (Ship, Movement) -> Bool
    isMovable r f (s, m) = canBeMoved s m r f


--------------------------------------------------------------------------------
-- * Firing shots
--------------------------------------------------------------------------------

{-
This is roughly how it works:
Each square is assigned a probability to be blocked (i.e. being part of no ship).
For example if we just hit a water cell, we know it's blocked. (Probablity 1)  
But after some time a ship can move there, so this probability declines over time.
(I modeled it as exponential decay.)
For each position, we count how many ships this square can be part of.
These ships are weighted depending on how likely their existence is, according to
the probabilities of blocked squares, and assigned a score.
A checkerboard pattern is applied and some randomness is added. This gives the
scores for each position. The heighest one is then chosen.

TODO: Don't try to sink the ships, if movable! Try to hit them as much as possible,
but only sink them at the end. Otherwise the remaining ships will have too much
space to move around.
-}

-- | Add some randomness to the scores.
randomize :: MonadRandom m => ScoreGrid -> m ScoreGrid
randomize grid = do
  newList <- mapM addRandom (elems grid)
  return $ listArray (bounds grid) newList where
    addRandom :: MonadRandom m => Score -> m Score
    addRandom x = (x *) `liftM` (getRandomR (0.9,1.1)) -- multiply by random number between 0.9 and 1.1 to add some randomization

-- | Assigns each cell a score. If it's high, it means that it's beneficial
-- | to attack this cell. On how this is calculated, see below.
scoreGrid :: (MonadRandom m, MonadState CleverAI m) => m ScoreGrid
scoreGrid = do
  CleverAI { .. } <- get
  return $ {- trace' showScoreGrid $ -} scoreGrid' rules tracking shots sunk sunkTime

scoreGrid' :: Rules -> TrackingGrid -> [Pos] -> FleetPlacement -> [Int] -> ScoreGrid
scoreGrid' rules tracking shots sunk sunkTime = array
  ((0, 0), (width-1, height-1))
  [(pos, scorePosition rules tracking shots sunk sunkTime remaining pos) -- scorePos (rulesSafetyMargin rules) tracking sunk remaining pos)
  | x <- [0..width-1]
  , y <-[0..height-1]
  , let pos = (x,y)] where
    (width, height) = rulesSize rules
    remaining = rulesShips rules \\ map shipSize sunk


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
  findEnd move pos = if inRange (bounds t) (move pos) && isHitOrSunk (t ! (move pos))
                         then findEnd move $ move pos else pos


-- | Given a position pos, assign it a score. This is calculated by
-- | counting how many ships pos can be part of, taking into account how
-- | likely their existence is, according to the AI's knowledge about
-- | the opponent's fleet.
-- | Also, the AI follows a checkerboard pattern, so it doesn't need to
-- | test all cells.
-- TODO: Adapt this function so that it doesn't always try to sink ships,
-- which makes things harder for itself because ships can move more freely.
scorePosition :: Rules -> TrackingGrid -> [Pos] -> FleetPlacement -> [Int] -> [Int] -> Pos -> Score
scorePosition rules tracking shots sunk sunkTime remaining pos@(x,y) =
  preventTwiceOnSameSpot
  . checkerboard $ sum (map (scoreShipsThrough pos) remaining) where
  
  preventTwiceOnSameSpot :: Score -> Score
  preventTwiceOnSameSpot s = if isHitOrSunk $ tracking ! pos then 0 else s

  checkerboard :: Score -> Score
  checkerboard = if even (x + y) then id else (*) 0.8

  scoreShipsThrough :: Pos -> Int -> Score
  scoreShipsThrough (sx,sy) len =
    sum . map scoreShip
    . filter (shipAdmissible rules [])
    $ [ShipShape (sx-dx, sy) len Horizontal | dx <- [0..len-1]]
      ++ [ShipShape (sx, sy-dy) len Vertical | dy <- [0..len-1]]

  probBlocked :: ScoreGrid
  probBlocked = probBlockedGrid rules tracking shots sunk sunkTime

  scoreShip :: ShipShape -> Score
  scoreShip s = hitBonus * probNotBlocked where
    probNotBlocked = product
                   . map ((1 -) . (probBlocked !))
                   $ coords
    hitBonus       = if hitCells == 0 then 1 else
                     if hitCells == 1 then 10
                     else                  50
    hitCells       = length . filter (isHit . (tracking !)) $ coords
    coords         = shipCoordinates 0 s


-- | Returns a grid where the value at each position is the estimated probability
-- | that this position is blocked, i.e. doesn't contain a ship.
-- | For example, when a ship was recently sunk, it's unlikely that there will be a ship at that spot.
-- | Or say, few moves ago, we hit water, then this won't change very soon.
-- | At least, that's the assumption.
probBlockedGrid :: Rules -> TrackingGrid -> [Pos] -> FleetPlacement -> [Int] -> ScoreGrid
probBlockedGrid rules tracking shots sunk sunkTime = array ((0, 0), (width - 1, height - 1))
  [(pos, probBlocked pos)
  | x <- [0..width-1]
  , y <-[0..height-1]
  , let pos = (x,y)] where
    (width, height)  = rulesSize rules
    probBlocked p = maximum [probWater p, probNearSunk p,  diagonalHit p]
    numMovesAgo    p = case p `elemIndex` shots of
      Nothing -> error "numMovesAgo with invalid position."
      Just x  -> x
    decayFactor      = if rulesMove rules then 0.99 else 1 :: Score
    -- | Probability for a ship to be on a (former) water cell
    probWater      p = if isWater $ tracking ! p
                       then decayFactor ^ (numMovesAgo p) -- exponential "decay"
                       else 0
    -- | Probability for a ship to be within the safety zone of a sunk ship.
    probNearSunk   p = maximum'
                     . map (\(_,c) -> decayFactor ^ (length shots - c))
                     . filter (\(s,_) -> p `elem` shipCoordinates (rulesSafetyMargin rules) s) -- is p within the safety zone of a ship
                     $ zip sunk sunkTime where
    -- | see below
    diagonalHit     p = const 0 p
{-
TODO: diagonalHit: In situations like this
???
?H?
???
we know that the positions marked with X are blocked for *sure*:
X?X
?H?
X?X
so diagonalHit should return 1
-}

maximum' :: (Ord a, Num a) => [a] -> a
maximum' [] = 0
maximum' xs = maximum xs

-------------------------------------------------------------------------------
-- * Serialization
-------------------------------------------------------------------------------

instance Serialize CleverAI where
  get = CleverAI <$> S.get <*> S.get <*> (fmap fromWord8s S.get) <*> S.get <*> S.get
  put CleverAI {..} = S.put rules
                    >> S.put tracking
                    >> S.put (toWord8s shots)
                    >> S.put sunk
                    >> S.put sunkTime

fromWord8s :: [(Word8,Word8)] -> [Pos]
fromWord8s = map (\(x,y) -> (fromIntegral x, fromIntegral y))

toWord8s :: [Pos] -> [(Word8,Word8)]
toWord8s  = map (\(x,y) -> (fromIntegral x, fromIntegral y))
