module GameLogic.TicTacToe
  ( Player (..)
  , TicTacToe (..)
  , Pos
  , initialField
  , getField
  , setField
  , moves
  , winner
  , player
  , positions
  , freePositions
  ) where

import Data.Bits hiding (Bits)
import Data.Maybe
import Prelude

--------------------------------------------------------------------------------
-- * Tic Tac Toe

-- | Tic tac toe player.
data Player = X | O deriving (Eq, Show)

-- | Bit field.
type Bits = Int

-- | Tic tac toe game state; first bitset for X, second for O.
data TicTacToe = TicTacToe !Bits !Bits deriving (Eq, Read)

-- | Position on the game field.
type Pos = (Int, Int)

instance Ord TicTacToe where
  compare f f' = compare (value f) (value f') where
    value field = case winner field of
      Just X  -> -1
      Just O  -> 1
      Nothing -> 0

--------------------------------------------------------------------------------
-- ** Pretty printing

instance Show TicTacToe where
  show field = fmap (toChar . getField field) positions where
    toChar (Just X) = 'X'
    toChar (Just O) = 'O'
    toChar Nothing  = '-'

--------------------------------------------------------------------------------
-- ** Operations

initialField :: TicTacToe
initialField = TicTacToe 0 0

-- | Updates a position of the field.
setField :: TicTacToe -> Player -> Pos -> TicTacToe
setField (TicTacToe x o) X pos = TicTacToe (setBit x $ posToBit pos) o
setField (TicTacToe x o) O pos = TicTacToe x (setBit o $ posToBit pos)

-- | Checks a position on the field.
getField :: TicTacToe -> Pos -> Maybe Player
getField (TicTacToe x o) pos
  | testBit x $ posToBit pos = Just X
  | testBit o $ posToBit pos = Just O
  | otherwise                = Nothing

-- | All possible moves of the current player.
moves :: TicTacToe -> [TicTacToe]
moves field
  | isJust $ winner field = []
  | otherwise             = fmap (setField field p) $ freePositions field where
    p = player field

-- | The current player.
player :: TicTacToe -> Player
player (TicTacToe x o) 
  | (countOnes $ x .|. o) `mod` 2 == 0 = X
  | otherwise                          = O

-- | All free positions on the field.
freePositions :: TicTacToe -> [Pos]
freePositions (TicTacToe x o) = filter (testBit bits . posToBit) positions where
  bits = complement $ x .|. o

-- | The winner of the field, if any.
winner :: TicTacToe -> Maybe Player
winner (TicTacToe x o)
  | hasWonBits x = Just X
  | hasWonBits o = Just O
  | otherwise    = Nothing

-- | All positions on the field.
positions :: [Pos]
positions = [(x, y) | y <- [0..2], x <- [0..2]]

--------------------------------------------------------------------------------
-- ** Internal operations

-- | Checks whether the player with that bitset has won.
hasWonBits :: Bits -> Bool
hasWonBits bits = not $ null $ filter (\b -> b == bits .&. b) winningBits

-- | Converts a position to the offset in the bitsets.
posToBit :: Pos -> Bits
posToBit (x, y) = x + (y * 3)

-- | Bit 
winningBits :: [Bits]
winningBits = [ 7, 56, 448, 73, 146, 292, 273, 84 ]

countOnes :: Bits -> Int
countOnes 0 = 0
countOnes x = countOnes (x `shiftR` 1) + if testBit x 0 then 1 else 0
