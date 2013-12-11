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
import           Data.List as L
import           Data.Maybe

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
placeRemainingShips (n:ns) r f = liftM2 (:) (placeShip n r f) (placeRemainingShips ns r f)

placeShip :: MonadRandom m => Int -> Rules -> m Fleet -> m Ship
placeShip n r f = do 
	                let s = getRandomShip (rulesSize r) n
	                adm <- liftM2 (shipAdmissible r) f s
	                if adm
	                  then s 
	                  else placeShip n r f

shipAdmissible :: Rules -> Fleet -> Ship -> Bool
shipAdmissible (Rules {..}) fleet ship = rangeCheck && freeCheck where
  rangeCheck     = L.all inRange shipCoords
  freeCheck      = L.all (isNothing . shipAt fleet) shipCoords
  inRange (x, y) = L.and [ 0 <= x, x < w, 0 <= y, y < h]
  (w, h)         = rulesSize
  shipCoords     = shipCoordinates ship

getRandomPos :: MonadRandom m => (Int,Int) -> m (Int,Int)
getRandomPos (w,h) = (,) `liftM` getRandomR (0,w-1) `ap` getRandomR (0,h-1)

getRandomShip :: MonadRandom m => (Int,Int) -> Int -> m Ship
getRandomShip size n = do
	                     (x,y) <- getRandomPos size
	                     return (Ship (x,y) n Horizontal)