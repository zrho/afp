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
initShips r = placeRemainingShips (rulesShips r) r (return [])

placeRemainingShips :: MonadRandom m => [Int] -> Rules -> m Fleet -> m Fleet
placeRemainingShips []     _ f = f
placeRemainingShips (n:ns) r f = placeRemainingShips ns r (liftM2 (:) (placeShip n r f) f)

placeShip :: MonadRandom m => Int -> Rules -> m Fleet -> m Ship
placeShip n r f = do 
	                let s = getRandomShip (rulesSize r) n
	                adm <- liftM2 (shipAdmissible r) f s
	                if adm
	                  then s 
	                  else placeShip n r f

getRandomPos :: MonadRandom m => (Int,Int) -> m (Int,Int)
getRandomPos (w,h) = (,) `liftM` getRandomR (0,w-1) `ap` getRandomR (0,h-1)

getRandomOrientation :: MonadRandom m => m Orientation
getRandomOrientation = do
	                     ori <- getRandomR (0,1)
	                     let orientation = ori == (0 :: Int)
	                     if orientation
	                     	then return Horizontal
	                     	else return Vertical

getRandomShip :: MonadRandom m => (Int,Int) -> Int -> m Ship
getRandomShip size n = do
	                     (x,y) <- getRandomPos size
	                     orientation <- getRandomOrientation
	                     return (Ship (x,y) n orientation)