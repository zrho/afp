{-# LANGUAGE RecordWildCards #-}
module Logic.Benchmark where

import           Control.Monad (foldM)
import           Control.Monad.Random
import           Control.Monad.State.Class (MonadState)
import           Control.Monad.Trans.State (runStateT)
import           Data.Array
import           Logic.Game
import           Logic.StupidAI
import           Logic.CleverAI
import           Logic.AIUtil
import           Data.Maybe
import           Prelude
import           Debug.Trace

verboseOutput :: String -> IO ()
verboseOutput = putStrLn -- to suppress verbose output, change to `const (return ())`

-- | Tests the performance of the AI, that is the average number of shots
-- | needed to sink all the ships. This way, we can estimate how well the AI plays.
-- | The AI plays against itself.
benchmark :: Int -> IO ()
benchmark repetitions = do
  total <- foldM f 0 [1..repetitions]
  let avg = (fromIntegral total :: Double) / fromIntegral repetitions
  putStrLn $ "Average: " ++ show avg ++ " shots." where
    f :: Int -> Int -> IO Int
    f total i = do
      verboseOutput $ "\n---\n" ++ show i ++ "th run:"
      shotCount <- playGame
      verboseOutput $ show shotCount ++ " shots needed."
      return (total + shotCount)

-- | Returns number of shots the AI needed.
playGame :: IO Int
playGame = do
  (ai, fleetPlacement) <- aiInit rules
  verboseOutput $ showFleetPlacement rules fleetPlacement
  let fleet = generateFleet fleetPlacement
  (count, _newAi) <- runStateT (aiTurn impact fleet 0) (ai :: CleverAI)
  return count where
    impact = newGrid (rulesSize rules) Nothing

-- | Let the AI play against itself. Returns the number of shots fired.
-- | TODO: Allow ships to be moved.
aiTurn :: (AI a, MonadRandom m, MonadState a m) => TrackingGrid -> Fleet -> Int -> m Int
aiTurn impact fleet count = do
    -- get target position from AI
    pos <- aiFire
    -- fire the AI's shot against itself
    let response = undefined -- fireAt fleet pos
    -- update the impact grid
    let newImpact   = impact // [(pos, Just response)]
    -- notify the AI
    aiResponse pos response
    -- all ships sunk now?
    case allSunk fleet of
      True  -> return (count + 1)
      False -> aiTurn newImpact fleet (count + 1)

rules :: Rules 
rules = Rules (10, 10) [ 5, 4, 4, 3, 3, 3, 2, 2, 2, 2 ] 1
