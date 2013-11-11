module GameLogic.Interaction
  ( userMove
  , computerMove
  , ended
  , won
  , draw
  ) where

import Data.Maybe (isJust)
import GameLogic.TicTacToe
import GameLogic.Search
import Prelude
import Control.Applicative

userMove :: TicTacToe -> Pos -> TicTacToe
userMove f p = case getField f p of
  Just _  -> f
  Nothing -> setField f (player f) p

computerMove :: TicTacToe -> TicTacToe
computerMove f = maybe f id $ nextDraw f where
  nextDraw = case comp of
    X -> nextDrawX
    O -> nextDrawO
  comp = player f

nextDrawX :: TicTacToe -> Maybe TicTacToe
nextDrawX
  = fmap (\(Node x _) -> x)
  . selectMinAB
  . prune 7
  . unfoldTree moves

nextDrawO :: TicTacToe -> Maybe TicTacToe
nextDrawO
  = fmap (\(Node x _) -> x)
  . selectMaxAB
  . prune 7
  . unfoldTree moves

ended :: TicTacToe -> Bool
ended = (||) <$> won <*> draw

won :: TicTacToe -> Bool
won = isJust . winner

draw :: TicTacToe -> Bool
draw = null . freePositions
