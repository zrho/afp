module Logic.StupidAI
  ( StupidAI
  ) where

-- external imports
import Prelude
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
-- internal imports
import Logic.GameFramework

data StupidAI = StupidAI { rules :: Rules }

instance AI StupidAI where
  aiInit r = return $ StupidAI r
  -- TODO: Implement a really stupid way of placing ships (like putting all in the upper left corner)
  aiPlaceFleet _ = return undefined
  aiFire = gets (mapSize . rules) >>= getRandomPos
  aiResponse _ = return ()

getRandomPos :: MonadRandom m => (Int,Int) -> m (Int,Int)
getRandomPos (w,h) = (,) `liftM` getRandomR (0,w-1) `ap` getRandomR (0,h-1)