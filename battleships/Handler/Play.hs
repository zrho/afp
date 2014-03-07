----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Play
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Handler for playing the battleships game.
--
-- Beside the GET handler for displaying the UI, two POST handlers for performing
-- the two actions of the game (moving and shooting) are provided. After a validation
-- step, these advance the simulation and redirect the user either to the game UI
-- again, if the game is still on, or to the game ended screen.

{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Handler.Play
  ( getPlayR
  , postMoveR
  , postFireR
  ) where

import Import
import Control.Monad.State
import Data.Map ((!))
import Data.Serialize (Serialize)
import Logic.Game
import Logic.GameExt
import Logic.Render
import Handler.Util
import Text.Julius (rawJS)

-------------------------------------------------------------------------------
-- * Forms
-------------------------------------------------------------------------------

fireForm :: FormInput Handler (Double, Double)
fireForm = (,) <$> ireq doubleField "X" <*> ireq doubleField "Y"

moveForm :: FormInput Handler (Maybe Double, Maybe Double)
moveForm = (,) <$> iopt doubleField "X" <*> iopt doubleField "Y"

-------------------------------------------------------------------------------
-- * Handler
-------------------------------------------------------------------------------

-- | Displays the game UI to the user.
getPlayR :: GameStateExt -> Handler Html
getPlayR gameE = withGame gameE $ \(gameState@GameState {..}) -> defaultLayout $ do
  setNormalTitle
  addScript $ StaticR js_jquery_js
  messageRender <- getMessageRender
  $(widgetFile "board")
  $(widgetFile "play")

-- | Handles a request to move one of the player's ships.
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
      game' <- execStateT (moveHuman pos >>= executeMove) game
      performAI game'

-- | Handles a request to fire at an enemy position.
postFireR :: GameStateExt -> Handler Html
postFireR gameE = withGame gameE $ \game -> do
  (x,y) <- runInputPost fireForm
  case fieldPos (x,y) of
    -- invalid click
    Nothing  -> invalidMove gameE
    -- valid click
    Just pos -> case expectedAction game of
      ActionMove -> invalidMove gameE
      ActionFire -> do
        (result, game') <- runStateT (humanTurnFire pos) game
        -- evaluate outcome
        case result of
          Over  -> gameEnded game'
          -- shoot again. expectedAction has not changed
          Again -> continue game'
          -- either perform AI turn or let human move
          Next
            | expectedAction game' == ActionMove -> continue game'
            | otherwise -> performAI game'

shipsOpponentWidget :: GameState a -> Orientation -> WidgetT App IO ()
shipsOpponentWidget gameState orientation = $(widgetFile "shipsOpponent")

legendWidget :: Orientation -> Bool -> Widget
legendWidget orientation movesAllowed = $(widgetFile "legend")

-------------------------------------------------------------------------------
-- * Redirections
-------------------------------------------------------------------------------

-- | Redirects to the game UI in case of an invalid move.
invalidMove :: GameStateExt -> Handler Html
invalidMove = getPlayR -- redirect . PlayR

-- | Redirects to the game ended screen.
gameEnded :: (Serialize a, AI a) => GameState a -> Handler Html
gameEnded game = expGameH game >>= redirect . GameEndedR

-- | Redirects the the game UI in case the game is still on.
continue :: (Serialize a, AI a) => GameState a -> Handler Html
continue game = expGameH game >>= getPlayR -- redirect . PlayR

-------------------------------------------------------------------------------
-- * AI
-------------------------------------------------------------------------------

-- | Performs the AI actions.
performAI :: (Serialize a, AI a) => GameState a -> Handler Html
performAI game = do
  (result, game') <- liftIO $ runStateT aiTurn game
  case result of
    Over  -> gameEnded game'
    Next  -> continue game'
    Again -> error "impossible. `Again` is handled by aiTurn"
