{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Logic.CleverAI (CleverAI) where

import           Prelude
import           Data.Array
import           Data.List ((\\), all, any, intersect)
import           Data.Maybe (fromJust, isJust)
import           Logic.Game
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Applicative
import           Data.Serialize (Serialize)
import qualified Data.Serialize as S
import           Debug.Trace

data CleverAI = CleverAI { rules :: Rules, tracking :: TrackingGrid }

type Score = Double
type ScoreGrid = Array Pos Score

type TrackingArray = Array Pos (Maybe HitResponse)

instance AI CleverAI where
  aiInit r       = liftM2 (,) (return (CleverAI r (newGrid (rulesSize r) Nothing, Nothing))) (initShips r)
  aiFire         = liftM chooseMaximum scoreGrid where
    chooseMaximum :: ScoreGrid -> Pos
    chooseMaximum arr = foldl1 (\maxP p -> if arr ! p > arr ! maxP then p else maxP) (indices arr)
  aiResponse p r = state updateTracking where
    updateTracking ai =
      let tracking' = (fst (tracking ai) // [(p, Just r)], Just p)
      in ((), ai { tracking = tracking' })

instance Serialize CleverAI where
  get = CleverAI <$> S.get <*> S.get
  put CleverAI {..} = S.put rules >> S.put tracking

--------------------------------------------------------------------------------
-- * Placing ships
--------------------------------------------------------------------------------

initShips :: MonadRandom m => Rules -> m Fleet
initShips r = do
  result <- initShips' r [] (rulesShips r)
  case result of
    Nothing -> undefined -- the grid is too small to place the fleet, there is no valid one
    Just fleet -> return fleet

-- | returns a random fleet, if one exists
initShips' :: MonadRandom m => Rules
                            -> Fleet
                            -> [Int] -- the sizes of the remaining ships to be placed
                            -> m (Maybe Fleet)
initShips' _ fleet []   = return . Just $ fleet
initShips' r fleet (len:lens) =
  let placements = admissibleShipPlacements r fleet len
  in tryPlacement placements where
    -- | Given a list of placements for the current ship,
    -- | try all of them in random order and choose the first one that works out.
    -- | Place the remaining fleet recursively.
    tryPlacement :: (MonadRandom m) => [(Pos, Orientation)] -> m (Maybe Fleet)
    tryPlacement []         = return Nothing -- No feasible placement for the current ship exists,
                                             -- so there is none for the whole fleet, either.
    tryPlacement placements = do
      ix <- getRandomR (0, length placements - 1)
      let (pos, orient) = placements !! ix
          newShip       = Ship pos len orient
      remainingShips <- initShips' r (newShip : fleet) lens -- place the remaining ships recursively
      case remainingShips of
        Just f -> return $ Just f
        Nothing    -> tryPlacement (removeNth ix placements) -- this placement doesn't work, try the next one

-- | calculates all possible placements (Pos, Orientation) for a ship of the given lengths
admissibleShipPlacements :: Rules -> Fleet -> Int -> [(Pos, Orientation)]
admissibleShipPlacements r fleet len = filter admissible allPlacements where
  admissible (pos, orient) = shipAdmissible r fleet $ Ship pos len orient
  (width, height) = rulesSize r
  allPlacements = [((x,y), orient) | x <- [0..width-1]
                                   , y <- [0..height-1]
                                   , orient <- [Horizontal, Vertical]
                                   ]

--------------------------------------------------------------------------------
-- * Firing shots
--------------------------------------------------------------------------------                                  

-- | Assigns each cell a score. If it's high, it means that it's beneficial
-- | to attack this cell. On how this is calculated, see below.
scoreGrid :: (MonadRandom m, MonadState CleverAI m) => m ScoreGrid
scoreGrid = do
  CleverAI {rules = r, tracking = (t,_)} <- get
  return $ trace' showScoreGrid $ scoreGrid' r t

scoreGrid' :: Rules -> TrackingArray -> ScoreGrid
scoreGrid' r t = array
  ((0, 0), (width-1, height-1))
  [(pos, scorePos (rulesSafetyMargin r) t sunk remaining pos)
  | x <- [0..width-1]
  , y <-[0..height-1]
  , let pos = (x,y)] where
    (width, height) = rulesSize r
    remaining = rulesShips r \\ map shipSize sunk
    sunk = if rulesSafetyMargin r < 1 -- the method of sunkFleet doesn't work in that case
      then []                         -- so we don't know
      else sunkFleet t [(x,y) | x <- [0..width-1], y <-[0..height-1]]

-- | Given a list of hit positions, find out which ships are sunk.
sunkFleet :: TrackingArray -> [Pos] -> Fleet
sunkFleet t = map (fromJust . findSunkShip t) . filter (isSunk . (t !))

-- | Given a position, look in all directions to find out whether it's part of a sunk ship.
findSunkShip :: TrackingArray -- ^ AI's tracking array
             -> Pos           -- ^ cell where the ship was sunk
             -> Maybe Ship    -- ^ sunk ship, if one exists
findSunkShip t p = findHorizontal `mplus` findVertical where
  findHorizontal =
    let left@(leftX,_) = findEnd (\(x,y) -> (x-1,y)) p
        (rightX,_)     = findEnd (\(x,y) -> (x+1,y)) p
        len            = rightX - leftX + 1
    in if len < 2 then Nothing else Just $ Ship left len Horizontal
  findVertical =
    let up@(_,upY) = findEnd (\(x,y) -> (x,y-1)) p
        (_,downY)  = findEnd (\(x,y) -> (x,y+1)) p
        len        = downY - upY + 1
    in if len < 2 then Nothing else Just $ Ship up len Vertical
  findEnd move p = if inRange (bounds t) (move p) && isHitOrSunk (t ! (move p))
                         then findEnd move $ move p else p


-- | Given a position p, assign it a score. This is calculated by
-- | counting how many ships p can be part of, according to the AI's
-- | knowledge about the opponent's fleet.
-- | Also, the AI follows a checkerboard pattern, so it doesn't need to
-- | test all cells.
-- TODO: If ships are movable, this function needs to be adapted because
-- the AI in its current state tries to sink as many ships possible, which
-- is then suboptimal as other ships can get more space to move.
scorePos :: Int           -- ^ safety margin from the rules
         -> TrackingArray -- ^ AI's tracking array
         -> Fleet         -- ^ already sunk fleet
         -> [Int]         -- ^ lengths of remaining ships
         -> Pos           -- ^ the position to be scored
         -> Score
scorePos _ t _ _ p | isJust (t ! p) = 0 -- don't fire at same spot twice
scorePos margin t sunk f p@(x,y) = 
  checkerboard . removeImpossible
  $ sum (map (scoreShipsThrough p) f) where
  checkerboard :: Score -> Score
  checkerboard = if even (x + y) then id else (0.9 *)
  removeImpossible :: Score -> Score
  removeImpossible = if p `elem` impossibleZones then const 0 else id
  scoreShipsThrough :: Pos -> Int -> Score
  scoreShipsThrough p@(x,y) len =
    sum . map scoreShip
    . filter (shipMatchesTracking margin t sunk)
    $ [Ship (x-dx, y) len Horizontal | dx <- [0..len-1]]
      ++ [Ship (x, y-dy) len Vertical | dy <- [0..len-1]]
  scoreShip :: Ship -> Score
  scoreShip s = if hitCells == 0 then 1 else
    if hitCells == 1 then 10 else 50 where -- score hit ships higher -> we want to sink them
      hitCells = length . filter (isHit . (t !)) $ coord
      coord    = shipCoordinates 0 s
  impossibleZones :: [Pos]
  impossibleZones = sunk >>= shipCoordinates margin

-- | Tests whether the ship can exist according to the AI's knowledge
-- | about the opponent's fleet.
-- | TODO: If ships can be moved, this function is too restrictive.
shipMatchesTracking :: Int           -- ^ safety margin from the rules
                    -> TrackingArray -- ^ AI's tracking field
                    -> Fleet         -- ^ already sunk fleet
                    -> Ship          -- ^ ship to be tested
                    -> Bool
shipMatchesTracking margin t sunk s = 
  (all (inRange (bounds t)) coord)          -- ship must lie within the field
  && (not . any isSunk $ coordTracking)     -- ship must not be already sunk
  && (any isHit coordTracking               -- ship must either be on a hit cell
    || null (coord `intersect` safetyZones) -- or none of its cells (including margin) must be hit
    )
  && (null (coord `intersect` sunkZones))   -- ship must not be within the safety margin of a sunk ship
  && (not . any isWater $ coordTracking)    -- none of its cells must be water
  -- TODO: some of these condition need not hold when the ships are movable:
  -- E.g. even if somewhere used to be water, a ship can have moved there
  where
    coordTracking = map (t !) coord
    coord = shipCoordinates 0 s
    safetyZones = filter (isHitOrSunk . (t !)) (indices t) >>= (addMargin margin)
    sunkZones = sunk >>= shipCoordinates margin

--------------------------------------------------------------------------------
-- * Helper functions
--------------------------------------------------------------------------------

isHit :: Maybe HitResponse -> Bool
isHit (Just Hit) = True
isHit _          = False

isSunk :: Maybe HitResponse -> Bool
isSunk (Just Sunk) = True
isSunk _           = False

isWater :: Maybe HitResponse -> Bool
isWater (Just Water) = True
isWater _            = False

isHitOrSunk :: Maybe HitResponse -> Bool
isHitOrSunk h = isSunk h || isHit h

addMargin :: Int -> Pos -> [Pos]
addMargin margin (x,y) = [(x + dx, y + dy) | dx <- [-margin..margin], dy <- [-margin..margin]]

-- can't find this function in the standard libraries...
removeNth :: Int -> [a] -> [a]
removeNth n xs = let (ys,zs) = splitAt n xs in ys ++ (tail zs)

--------------------------------------------------------------------------------
-- * Debugging
--------------------------------------------------------------------------------

showPositions :: Int -> Int -> [Pos] -> String
showPositions width height ps = concat
  [(if (x,y) `elem` ps then "X" else " ")
  ++ (if x == width - 1 then "\n" else "")
  | y <- [0..height - 1]
  , x <- [0..width - 1]
  ]

showScoreGrid :: ScoreGrid -> String
showScoreGrid grid = concat
  [ show (round $ grid ! (x,y))
  ++ (if x == width' then "\n" else "|")
  | y <- [0..height']
  , x <- [0..width']
  ] where
    ((0,0), (width', height')) = bounds grid

trace' :: (a -> String) -> a -> a
trace' f x = trace (f x) x
