module Logic.AIUtil where

import           Prelude
import           Logic.Game
import           Logic.Random
import           Data.Array
import           Data.Maybe (isJust)
import qualified Data.Map as Map
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.List
import           Control.Monad.Random
import           Logic.Debug
import           Text.Printf

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
  fleets <- runRandM $ runListT $ foldM (initShips' r) [] (rulesShips r)
  return $ head $ fleets

-- | returns a random fleet, if one exists
initShips'
  :: (MonadRandom m)
  => Rules
  -> FleetPlacement
  -> Int
  -> ListT m FleetPlacement

initShips' r fleet len = do
  let admissible = admissibleShipPlacements r fleet len
  shuffled  <- lift $ shuffleRandom admissible
  placement <- choose shuffled
  return $ placement : fleet

-- | calculates all possible placements for a ship of the given lengths
admissibleShipPlacements :: Rules -> FleetPlacement -> Int -> [ShipShape]
admissibleShipPlacements r fleet len = filter (shipAdmissible r fleet) allPlacements where
  (width, height) = rulesSize r
  allPlacements =
    [ ShipShape (x,y) len orient
    | x <- [0..width-1]
    , y <- [0..height-1]
    , orient <- [Horizontal, Vertical]
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

choose :: MonadPlus m => [a] -> m a
choose = msum . fmap return

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
  [ printf "%6.2f" (grid ! (x,y))
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

showFleetPlacement :: Rules -> FleetPlacement -> String
showFleetPlacement r fleet = tail $ concat
  [ (if x == 0 then "\n" else "") ++
    (if isJust . shipAt fleet $ (x,y) then "O" else "~")
  | y <- [0..height - 1]
  , x <- [0..width - 1]
  ] where
     (width, height) = rulesSize r

showFleet :: Rules -> Fleet -> String
showFleet r = showFleetPlacement r . map shipShape . Map.elems

trace' :: (a -> String) -> a -> a
trace' f x = debug (f x) x
