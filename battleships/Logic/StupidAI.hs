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
  aiInit r       = liftM2 (,) (return (StupidAI r)) (initShips r)
  aiFire         = gets (rulesSize . rules) >>= getRandomPos
  aiResponse _ _ = return ()

instance Serialize StupidAI where
  get = StupidAI <$> S.get
  put StupidAI {..} = S.put rules

initShips :: MonadRandom m => Rules -> m Fleet
initShips r = foldM f [] $ rulesShips r where
  f fleet len = liftM2 (:) (placeShip len r fleet) (return fleet)

placeShip :: MonadRandom m => Int -> Rules -> Fleet -> m Ship
placeShip len r f = do
  ship <- getRandomShip (rulesSize r) len
  case shipAdmissible r f ship of
    True  -> return ship
    False -> placeShip len r f

getRandomPos :: MonadRandom m => (Int, Int) -> m (Int,Int)
getRandomPos (w,h) = liftM2 (,) (getRandomR (0, w - 1)) (getRandomR (0, h - 1))

getRandomShip :: MonadRandom m => (Int, Int) -> Int -> m Ship
getRandomShip size len = liftM3 Ship (getRandomPos size) (return len) getRandom
