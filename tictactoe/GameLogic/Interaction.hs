module GameLogic.Interaction
  ( userMove
  , computerMove
  , ended
  ) where

import Data.Maybe (isJust)
import GameLogic.TicTacToe
import GameLogic.Search
import Prelude

userMove :: TicTacToe -> Pos -> TicTacToe
userMove f p = case getField f p of
  Just _  -> f
  Nothing -> setField f (player f) p

computerMove :: TicTacToe -> TicTacToe
computerMove f = case nextDraw f of
  Just f' -> f'
  Nothing -> f

nextDraw :: TicTacToe -> Maybe TicTacToe
nextDraw
  = fmap (\(Node x _) -> x)
  . selectMaxAB
  . prune 7
  . unfoldTree moves

ended :: TicTacToe -> Bool
ended f = isJust (winner f) || null (freePositions f)
