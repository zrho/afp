module Logic.GameFramework where

import Prelude
import Data.Array
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

class AI a where
  aiInit       :: MonadRandom m => Rules -> m a
  aiPlaceFleet :: (MonadRandom m, MonadState a m) => [Int] -> m Fleet
  aiFire       :: (MonadRandom m, MonadState a m) => m Pos
  aiResponse   :: (MonadRandom m, MonadState a m) => (Pos, HitResponse) -> m ()


data Rules = Rules { mapSize :: (Int, Int) }

data HitResponse = Water | Hit | Sunk deriving (Show, Eq, Ord, Bounded, Enum)

data Orientation = Horizontal | Vertical deriving (Show, Eq, Ord, Bounded, Enum)

data Ship = Ship 
            { position    :: Pos
            , size        :: Int
            , orientation :: Orientation
            } deriving (Show, Eq)

newtype Fleet = Fleet [Ship] deriving (Show, Eq)

type Pos = (Int,Int)

type TrackingGrid = Array Pos (Maybe HitResponse)