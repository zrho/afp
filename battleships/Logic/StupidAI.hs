{-# LANGUAGE RecordWildCards #-}
module Logic.StupidAI (StupidAI) where

import           Prelude
import           Logic.Game
import           Logic.AIUtil
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Applicative
import           Data.Maybe (fromJust)
import           Data.Serialize (Serialize)
import qualified Data.Serialize as S

data StupidAI = StupidAI { rules :: Rules }

instance AI StupidAI where
  aiInit r       = do
    ships <- liftM fromJust $ initShips r []
    return $ (StupidAI r, ships)
  aiFire         = gets (rulesSize . rules) >>= getRandomPos
  aiResponse _ _ = return ()

instance Serialize StupidAI where
  get = StupidAI <$> S.get
  put StupidAI {..} = S.put rules

getRandomPos :: MonadRandom m => (Int, Int) -> m (Int,Int)
getRandomPos (w,h) = liftM2 (,) (getRandomR (0, w - 1)) (getRandomR (0, h - 1))
