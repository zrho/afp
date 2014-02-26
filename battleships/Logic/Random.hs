module Logic.Random
  ( extractRandom
  , shuffleRandom
  , chooseRandom
  , runRandM
  ) where

import Prelude
import Control.Monad
import Control.Monad.Random

-- | Extracts a random element from a list.
extractRandom :: MonadRandom m => [a] -> m (Maybe (a, [a]))
extractRandom [] = return Nothing
extractRandom xs = do
  ix <- getRandomR (0, length xs - 1)
  let (ys, z : zs) = splitAt ix xs
  return $ Just (z, ys ++ zs)

-- | Randomly permutes an array.
shuffleRandom :: MonadRandom m => [a] -> m [a]
shuffleRandom xs = extractRandom xs >>= \r -> case r of
  Nothing      -> return []
  Just (y, ys) -> liftM (y :) $ shuffleRandom ys

chooseRandom :: MonadRandom m => [a] -> m (Maybe a)
chooseRandom = (liftM . liftM) fst . extractRandom

runRandM :: MonadRandom m => Rand StdGen a -> m a
runRandM rand = do
  i <- getRandom
  return $ fst $ runRand rand $ mkStdGen i