----------------------------------------------------------------------------
-- |
-- Module      :  Logic.Util
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities.

module Logic.Util
  (
    maximumIx
  , buildArray
  , traverseArray
  ) where

import           Prelude
import           Data.Array
import           Data.List
import           Data.Function
import qualified Data.Traversable (mapM)
import           Control.Arrow

--------------------------------------------------------------------------------
-- * Array Utilities
--------------------------------------------------------------------------------

-- | Index of the maximum element in an array.
maximumIx :: (Ix i, Ord e) => Array i e -> i
maximumIx = fst . maximumBy (compare `on` snd) . assocs

-- | Build an array by specifying bounds and a function that constructs
-- the value for an index.
buildArray :: Ix i => (i, i) -> (i -> a) -> Array i a
buildArray bs f = array bs $ fmap (id &&& f) $ range bs

-- | Distributive law for monad actions in arrays.
traverseArray :: (Ix i, Monad m) => (a -> m b) -> Array i a -> m (Array i b)
traverseArray = Data.Traversable.mapM
