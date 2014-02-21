{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Handler.Play where

import Import
import Control.Monad.State
import Data.Map ((!))
import Data.Serialize (Serialize)
import Logic.Game
import Logic.GameExt
import Handler.Util
import Handler.Grid

fireForm :: FormInput Handler (Double, Double)
fireForm = (,) <$> ireq doubleField "X" <*> ireq doubleField "Y"

moveForm :: FormInput Handler (Maybe Double, Maybe Double)
moveForm = (,) <$> iopt doubleField "X" <*> iopt doubleField "Y"

getPlayR :: GameStateExt -> Handler Html
getPlayR gameE = withGame gameE $ \(gameState@GameState {..}) -> defaultLayout $ do
  setNormalTitle
  addScript $ StaticR js_jquery_js
  $(widgetFile "play")

postMoveR :: GameStateExt -> Handler Html
postMoveR gameE = withGame gameE $ \game -> do
    mpos <- runInputPost moveForm
    case mpos of 
      (Just x, Just y) -> case fieldPos (x,y) of
        -- invalid click
        Nothing  -> invalidMove gameE
        -- valid click
        Just pos -> do 
          let humanFleet = playerFleet $ currentPlayer game
          case expectedAction game of
            ActionFire -> invalidMove gameE
            ActionMove -> case desiredMove pos humanFleet of
              Just (ship,movement) 
                | isMovable movement humanFleet (humanFleet!ship)
                  -> performMove game (Just pos)
              _   -> invalidMove gameE
      -- player skips moving
      _ -> performMove game Nothing
  where
    performMove game pos = do
      game' <- liftIO $ execStateT (executeMove $ moveHuman pos) game
      performAI game'

postFireR :: GameStateExt -> Handler Html
postFireR gameE = withGame gameE $ \game -> do
  (x,y) <- runInputPost fireForm
  case fieldPos (x,y) of
    -- invalid click
    Nothing  -> invalidMove gameE
    -- valid click
    Just pos -> do 
      case expectedAction game of
        ActionMove -> invalidMove gameE
        ActionFire -> do
          (result, game') <- liftIO $ runStateT (humanTurnFire pos) game
          -- evaluate outcome
          case result of
            Won   -> gameEnded game'
            -- shoot again. expectedAction has not changed
            Again -> continue game'
            -- either perform AI turn or let human move
            Next
              | expectedAction game' == ActionMove -> continue game'
              | otherwise -> performAI game'

invalidMove :: GameStateExt -> Handler Html
invalidMove gameE = redirect $ PlayR gameE

gameEnded, continue, performAI :: (Serialize a, AI a) => GameState a -> Handler Html
gameEnded game = expGameH game >>= redirect . GameEndedR
continue game = expGameH game >>= redirect . PlayR
performAI game = do
  (result, game') <- liftIO $ runStateT aiTurn game
  case result of
    Won   -> gameEnded game'
    Next  -> continue game'
    Again -> error "impossible. `Again` is handled by aiTurn"

shipsOpponentWidget :: GameState a -> Orientation -> WidgetT App IO ()
shipsOpponentWidget gameState orientation = $(widgetFile "shipsOpponent")
