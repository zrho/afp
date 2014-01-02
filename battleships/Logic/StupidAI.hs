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
initShips r = do
  result <- initShips' r [] (rulesShips r)
  case result of
    Nothing -> undefined -- the grid is too small to place the fleet, there is no valid one
    Just fleet -> return fleet

-- | returns a random fleet, if one exists
initShips' :: MonadRandom m => Rules
                            -> Fleet
                            -> [Int] -- the sizes of the remaining ships to be placed
                            -> m (Maybe Fleet)
initShips' r fleet []   = return . Just $ fleet
initShips' r fleet (len:lens) =
  let placements = admissibleShipPlacements r fleet len
  in tryPlacement placements where
    -- | Given a list of placements for the current ship,
    -- | try all of them in random order and choose the first one that works out.
    -- | Place the remaining fleet recursively.
    tryPlacement :: (MonadRandom m) => [(Pos, Orientation)] -> m (Maybe Fleet)
    tryPlacement []         = return Nothing -- No feasible placement for the current ship exists,
                                             -- so there is none for the whole fleet, either.
    tryPlacement placements = do
      index <- getRandomR (0, length placements - 1)
      let (pos, orient) = placements !! index
          newShip       = Ship pos len orient
      remainingShips <- initShips' r (newShip : fleet) lens -- place the remaining ships recursively
      case remainingShips of
        Just f -> return $ Just f
        Nothing    -> tryPlacement (removeNth index placements) -- this placement doesn't work, try the next one

-- | calculates all possible placements (Pos, Orientation) for a ship of the given lengths
admissibleShipPlacements :: Rules -> Fleet -> Int -> [(Pos, Orientation)]
admissibleShipPlacements r fleet len = filter admissible allPlacements where
  admissible (pos, orient) = shipAdmissible r fleet $ Ship pos len orient
  (width, height) = rulesSize r
  allPlacements = [((x,y), orient) | x <- [0..width-1]
                                   , y <- [0..height-1]
                                   , orient <- [Horizontal, Vertical]
                                   ]

getRandomPos :: MonadRandom m => (Int, Int) -> m (Int,Int)
getRandomPos (w,h) = liftM2 (,) (getRandomR (0, w - 1)) (getRandomR (0, h - 1))

-- can't find this function in the standard libraries...
removeNth :: Int -> [a] -> [a]
removeNth n xs = let (ys,zs) = splitAt n xs in ys ++ (tail zs)