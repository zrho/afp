module Main
  ( main
  , playGame
  , benchmark
  ) where

import           Prelude
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State (runStateT, StateT)
import           Data.List (sort)
import qualified Data.Map as Map
import           Logic.AIUtil
import           Logic.CleverAI
import           Logic.Game
import           Logic.Types
import           System.Environment

main :: IO ()
main = do
  putStrLn "Usage: aibenchmark (mov|immov) (Hard|Medium|Easy) numGames [--verbose]"
  putStrLn "Example: aibenchmark immov Hard 20 --verbose"
  a1:a2:a3:rest <- getArgs
  let
    moveable    = case a1 of
      "mov"   -> True
      "immov" -> False
      _       -> error $ "Error: `" ++ a2 ++ "` is neither `mov` nor `immov`!"
    difficultyLvl = read a2
    repetitions   = read a3
    verbose       = case rest of
      "--verbose":_ -> True
      _      -> False
  benchmark
    verbose
    benchmarkRules { rulesMove = moveable, rulesDifficulty = difficultyLvl }
    repetitions

-- | Tests the performance of the AI, that is the average number of shots
-- | needed to sink all the ships. This way, we can estimate how well the AI plays.
-- | The AI plays against itself.
benchmark :: Bool -> Rules -> Int -> IO ()
benchmark verbose rules repetitions = do
  shotCounts <- mapM f [1..repetitions]
  let sorted = sort shotCounts
  let avg = (fromIntegral $ sum shotCounts :: Double) / fromIntegral (length shotCounts)
  let mn = minimum shotCounts
  let mx = maximum shotCounts
  let mdn = sorted !! (length sorted `div` 2)
  putStrLn   "----------\n"
  putStrLn   "SUMMARY\n"
  putStrLn $ "Average: " ++ show avg ++ " shots."
  putStrLn $ "Minimum: " ++ show mn ++ " shots."
  putStrLn $ "Maximum: " ++ show mx ++ " shots."
  putStrLn $ "Median:  " ++ show mdn ++ " shots."
  putStrLn   "Complete list: "
  mapM_ print sorted where
    f :: Int -> IO Int
    f i = do
      putStrLn $ "\n---\n" ++ show i ++ "th run:"
      shotCount <- playGame verbose rules
      putStrLn $ show shotCount ++ " shots needed."
      return shotCount

-- | Returns number of shots the AI needed.
playGame :: Bool -> Rules -> IO Int
playGame verbose rules = do
  (ai, fleetPlacement) <- aiInit rules
  putStrLn $ showFleetPlacement fleetPlacement
  let fleet = generateFleet fleetPlacement
  (count, _newAi) <- runStateT (turn verbose rules [] fleet Map.empty 0) (ai :: CleverAI)
  return $ count `div` 2 -- only count AI's shots, not the turn number

-- | Let the AI play against itself. Returns the number of shots fired.
turn :: AI a => Bool -> Rules -> TrackingList -> Fleet -> Fleet -> Int -> StateT a IO Int
turn verbose rules shots fleet sunk count = do
    when verbose . liftIO $ do
      putStrLn "Fleet:"
      putStrLn $ showFleet fleet
      putStrLn "Sunk:"
      putStrLn $ showFleet sunk
      putStrLn "Tracking:"
      putStrLn $ showTrackingList shots
    -- get target position from AI
    pos <- aiFire
    when verbose . liftIO . putStrLn $ "AI's target: " ++ show pos
    -- fire the AI's shot against itself
    let
      (response, fleet', sunk') = case shipsAt (fleet Map.\\ sunk) pos of
        []     -> (Water, fleet, sunk)
        [ship] ->
          let
            -- inflict damage to the ship
            Just idx = shipCellIndex pos ship
            newShip  = damageShip count idx ship
            -- replace ship
            newFleet = Map.insert (shipID ship) newShip fleet
          in if isShipSunk newShip
             then (Sunk, newFleet, Map.insert (shipID ship) newShip sunk)
             else (Hit, newFleet, sunk)
        _      -> error $ "Error: Multiple ships at position " ++ show pos ++ ". This shouldn't happen!"
    -- update the tracking list
    let shots'   = Shot pos response count:shots
    -- notify the AI
    aiResponse pos response
    -- should any ships be moved?
    fleet'' <- if rulesMove rules
      then do
        mMove <- aiMove fleet' shots'
        when verbose . liftIO . putStrLn $ "AI's movement: " ++ show mMove
        return $ case mMove of
          Nothing -> fleet'
          Just (sID, movement) -> case Map.lookup sID fleet' of
            Just ship -> if not $ isDamaged ship
              then moveShip ship movement fleet'
              else fleet'
            Nothing   -> fleet'
      else return fleet'
    -- all ships sunk now?
    if allSunk fleet''
      then return (count + 2)
      else turn verbose rules shots' fleet'' sunk' (count + 2)

benchmarkRules :: Rules
benchmarkRules = Rules
  { rulesAgainWhenHit   = False
  , rulesMove           = undefined
  , rulesDifficulty     = undefined
  , rulesMaximumTurns   = 150
  , rulesCountdownTurns = 40
  }
