----------------------------------------------------------------------------
-- |
-- Module      :  Logic.AIUtil
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for the enemy AI.

module Logic.AIUtil
  (
  -- * Type Synonyms
    Score
  , ScoreGrid
  , TrackingGrid
  -- * Grid Functions
  , isHit
  , isHitOrSunk
  , isWater
  -- * Ship Functions
  , completeFleet
  -- * Helper Functions
  , buildArray
  , traverseArray
  , fromBool
  , maximum'
  , maximumIx
  -- * Debugging Functions
  , showFleet
  , showFleetPlacement
  , showPositions
  , showScoreGrid
  , showTracking
  , showTrackingList
  ) where

import           Prelude
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.List
import           Control.Monad.Random
import           Data.Array
import           Data.Function
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe (isJust, listToMaybe)
import qualified Data.Traversable (mapM)
import           Logic.Game
import           Logic.Random
import           Logic.Types
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

-- | Random completion of a given fleet, if one exists.
completeFleet :: MonadRandom m => FleetPlacement -> m (Maybe FleetPlacement)
completeFleet fleet = do
  let ships' = reverse fleetShips \\ fmap shipSize fleet
  fleets <- runRandM $ runListT $ foldM completeFleet' fleet ships'
  return $ listToMaybe fleets

-- | Helper for 'completeFleet'.
completeFleet'
  :: RandomGen g
  => FleetPlacement
  -> Int
  -> ListT (Rand g) FleetPlacement

completeFleet' fleet len = do
  let admissible = admissibleShips fleet len
  shuffled  <- lift $ shuffleRandom admissible
  placement <- choose shuffled
  return $ placement : fleet

-- | Calculates all possible placements for a ship of the given length.
admissibleShips :: FleetPlacement -> Int -> [ShipShape]
admissibleShips fleet len = do
  let (w, h) = boardSize
  x <- [0 .. w - 1]
  y <- [0 .. h - 1]
  o <- [Horizontal, Vertical]
  let ship = ShipShape (x, y) len o
  guard $ shipAdmissible fleet ship
  return ship

--------------------------------------------------------------------------------
-- * Hit Response
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
isHitOrSunk h = isSunk h || isHit h  -- equivalent: isJust h && not (isWater h)

--------------------------------------------------------------------------------
-- * Array Utilities
--------------------------------------------------------------------------------

-- | Index of the maximum element in an array.
maximumIx :: (Ix i, Ord e) => Array i e -> i
maximumIx = fst . maximumBy (compare `on` snd) . assocs

-- | Distributive law for monad actions in arrays.
traverseArray :: (Ix i, Monad m) => (a -> m b) -> Array i a -> m (Array i b)
traverseArray = Data.Traversable.mapM

-- | Build an array by specifying bounds and a function that constructs
-- the value for an index.
buildArray :: Ix i => (i, i) -> (i -> a) -> Array i a
buildArray bs f = array bs $ fmap (id &&& f) $ range bs

--------------------------------------------------------------------------------
-- * Misc
--------------------------------------------------------------------------------

-- | Lift a list into a 'MonadPlus'.
choose :: MonadPlus m => [a] -> m a
choose = msum . fmap return

-- | Bool to numeric.
fromBool :: Num a => Bool -> a
fromBool True  = 1
fromBool False = 0

-- | Maximum function for nonnegative numbers which handles empty lists.
maximum' :: (Ord a, Num a) => [a] -> a
maximum' [] = 0
maximum' xs = maximum xs

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

showTrackingList :: TrackingList -> String
showTrackingList list = showTracking . buildArray ((0,0), (fst boardSize - 1, snd boardSize - 1)) $
  \pos -> shotResult `fmap` find ((== pos) . shotPos) list

showFleetPlacement :: FleetPlacement -> String
showFleetPlacement fleet = tail $ concat
  [ (if x == 0 then "\n" else "") ++
    (if isJust . shipAt fleet $ (x,y) then "O" else "~")
  | y <- [0..height - 1]
  , x <- [0..width - 1]
  ] where (width, height) = boardSize

showFleet :: Fleet -> String
showFleet = showFleetPlacement . map shipShape . Map.elems
