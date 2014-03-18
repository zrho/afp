----------------------------------------------------------------------------
-- |
-- Module      :  Logic.StupidAI
-- Stability   :  experimental
-- Portability :  semi-portable
--
-- AI that randomly shoots and does not move.

module Logic.StupidAI 
  ( StupidAI
  ) where

import           Prelude
import           Control.Monad
import           Control.Monad.Random
import           Data.Maybe (fromJust)
import           Data.Serialize (Serialize(..))
import           Logic.AIUtil
import           Logic.Game
import           Logic.Types

data StupidAI = StupidAI

instance AI StupidAI where
  aiInit _       = do
    ships <- liftM fromJust $ completeFleet []
    return (StupidAI, ships)
  aiFire         = getRandomPos boardSize
  aiResponse _ _ = return ()

instance Serialize StupidAI where
  get = return StupidAI
  put StupidAI = return ()

getRandomPos :: MonadRandom m => (Int, Int) -> m (Int,Int)
getRandomPos (w,h) = liftM2 (,) (getRandomR (0, w - 1)) (getRandomR (0, h - 1))
