module Logic.AIUtil where

import           Prelude
import           Logic.Game
import           Data.Array
import           Data.Maybe (isJust)
import           Control.Monad.Random
import           Debug.Trace

--------------------------------------------------------------------------------
-- * Data types
--------------------------------------------------------------------------------

type Score = Double
type ScoreGrid = Array Pos Score

-- | A grid where the results of shots are tracked.
type TrackingGrid = Grid (Maybe HitResponse)

--------------------------------------------------------------------------------
-- * Placing ships
--------------------------------------------------------------------------------

initShips :: MonadRandom m => Rules -> m FleetPlacement
initShips r = do
  result <- initShips' r [] (rulesShips r)
  case result of
    Nothing -> undefined -- the grid is too small to place the fleet, there is no valid one
    Just fleet -> return fleet

-- | returns a random fleet, if one exists
initShips' :: MonadRandom m => Rules
                            -> FleetPlacement
                            -> [Int] -- the sizes of the remaining ships to be placed
                            -> m (Maybe FleetPlacement)
initShips' _ fleet []   = return . Just $ fleet
initShips' r fleet (len:lens) =
  let placements = admissibleShipPlacements r fleet len
  in tryPlacement placements where
    -- | Given a list of placements for the current ship,
    -- | try all of them in random order and choose the first one that works out.
    -- | Place the remaining fleet recursively.
    tryPlacement :: (MonadRandom m) => [ShipShape] -> m (Maybe FleetPlacement)
    tryPlacement []         = return Nothing -- No feasible placement for the current ship exists,
                                             -- so there is none for the whole fleet, either.
    tryPlacement placements = do
      ix <- getRandomR (0, length placements - 1)
      let newShip       = placements !! ix
      remainingShips <- initShips' r (newShip : fleet) lens -- place the remaining ships recursively
      case remainingShips of
        Just f  -> return $ Just f
        Nothing -> tryPlacement (removeNth ix placements) -- this placement doesn't work, try the next one

-- | calculates all possible placements for a ship of the given lengths
admissibleShipPlacements :: Rules -> FleetPlacement -> Int -> [ShipShape]
admissibleShipPlacements r fleet len = filter (shipAdmissible r fleet) allPlacements where
  (width, height) = rulesSize r
  allPlacements = [ShipShape (x,y) len orient
                  | x <- [0..width-1]
                  , y <- [0..height-1]
                  , orient <- [Horizontal, Vertical
                  ]
                                   ]


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

showTracking :: TrackingGrid -> String
showTracking grid = concat
  [(case grid ! ((x,y) :: Pos) of
      Nothing -> " "
      Just Water -> "~"
      Just Hit -> "H"
      Just Sunk -> "S")
  ++ (if x == width' then "\n" else "")
  | y <- [0..height']
  , x <- [0..width']
  ] where
    ((0,0), (width', height')) = bounds grid

trace' :: (a -> String) -> a -> a
trace' f x = trace (f x) x
showFleetPlacement :: Rules -> FleetPlacement -> String
showFleetPlacement r fleet = tail $ concat
  [ (if x == 0 then "\n" else "") ++
    (if isJust . shipAt fleet $ (x,y) then "O" else "~")
  | y <- [0..height - 1]
  , x <- [0..width - 1]
  ] where
     (width, height) = rulesSize r
