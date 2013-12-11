{-# LANGUAGE RecordWildCards #-}
module Logic.StupidAI (StupidAI) where

import           Prelude
import           Logic.Game
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Applicative
import           Data.Serialize (Serialize)
import qualified Data.Serialize as S

data StupidAI = StupidAI { rules :: Rules }

instance AI StupidAI where
  -- TODO: Implement a really stupid way of placing ships (like putting all in the upper left corner)
  aiInit r       = return (StupidAI r, initShips)
  aiFire         = gets (rulesSize . rules) >>= getRandomPos
  aiResponse _ _ = return ()

instance Serialize StupidAI where
  get = StupidAI <$> S.get
  put StupidAI {..} = S.put rules

initShips :: Fleet
initShips = [Ship (1,1) 4 Horizontal, Ship (7,8) 2 Vertical, Ship (5, 3) 5 Vertical]

getRandomPos :: MonadRandom m => (Int,Int) -> m (Int,Int)
getRandomPos (w,h) = (,) `liftM` getRandomR (0,w-1) `ap` getRandomR (0,h-1)