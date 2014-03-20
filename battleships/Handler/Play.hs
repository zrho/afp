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

module Handler.Play
  ( getPlayR
  , playView
  , postMoveR
  , postFireR
  ) where

import Import
import Control.Applicative
import Control.Monad.State
import Data.List (nub)
import Data.Map ((!))
import Data.Serialize (Serialize)
import Handler.Util
import Handler.GameEnded
import Logic.Game
import Logic.GameExt
import Logic.Render
import Logic.Types
import Text.Julius (rawJS)

-------------------------------------------------------------------------------
-- * Forms
-------------------------------------------------------------------------------

-- | A form with two required double fields named "X" and "Y".
fireForm :: FormInput Handler (Double, Double)
fireForm = (,) <$> ireq doubleField "X" <*> ireq doubleField "Y"

-- | A form with two optional double fields named "X" and "Y".
moveForm :: FormInput Handler (Maybe (Double, Double))
moveForm = liftA2 (liftA2 (,)) (iopt doubleField "X") (iopt doubleField "Y")

-------------------------------------------------------------------------------
-- * Handler
-------------------------------------------------------------------------------

-- | Displays the game UI to the user.
getPlayR :: Bool -> GameStateExt -> Handler Html
getPlayR firstContact gameE = withGame gameE $ \game -> playView firstContact game gameE

playView :: Bool -> GameState a -> GameStateExt -> Handler Html
playView firstContact game@GameState{..} gameE = defaultLayout $ do
  setNormalTitle
  addScript $ StaticR js_jquery_js
  messageRender <- getMessageRender
  let remTurns = remainingTurns game `div` 2
  let showAlert = firstContact &&
                  case expectedAction of ActionFire -> isCountdownStart game
                                         _          -> False
  $(widgetFile "board")
  $(widgetFile "play")

-- | Handles a request to move one of the player's ships.
postMoveR :: GameStateExt -> Handler Html
postMoveR gameE = withGame gameE $ \game -> do
    mpos <- runInputPost moveForm
    case mpos of 
      Just (x, y) -> case fieldPos (x,y) of
        -- invalid click
        Nothing  -> invalidMove game gameE
        -- valid click
        Just pos -> do 
          let humanFleet = playerFleet $ currentPlayer game
          case expectedAction game of
            ActionFire -> invalidMove game gameE
            ActionMove -> case desiredMove pos humanFleet of
              Just (ship,movement) 
                | isMovable movement humanFleet (humanFleet!ship)
                  -> performMove game (Just pos)
              _   -> invalidMove game gameE
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
    Nothing  -> invalidMove game gameE
    -- valid click
    Just pos -> case expectedAction game of
      ActionMove -> invalidMove game gameE
      ActionFire -> do
        (result, game') <- runStateT (humanTurnFire pos) game
        -- evaluate outcome
        case result of
          Over  -> gameEnded game'
          -- shoot again. expectedAction has not changed
          Again -> continue False game'
          -- either perform AI turn or let human move
          Next
            | expectedAction game' == ActionMove -> continue False game'
            | otherwise -> performAI game'

-- | A widget that displays a table showing which ships of the opponent are remaining.
shipsOpponentWidget :: GameState a -> Orientation -> WidgetT App IO ()
shipsOpponentWidget game orientation =
  let sizes = sizesOfShips $ unsunkShips $ playerFleet $ otherPlayer game
  in $(widgetFile "shipsOpponent")

-- | A widget that renders the legend in the given orientation.
legendWidget :: Orientation -> Bool -> Widget
legendWidget orientation movesAllowed = $(widgetFile "legend")

-------------------------------------------------------------------------------
-- * Redirections
-------------------------------------------------------------------------------

-- | Redirects to the game UI in case of an invalid move.
invalidMove :: GameState a -> GameStateExt -> Handler Html
invalidMove = playView False -- \_ -> redirect . PlayR False

-- | Redirects to the game ended screen.
gameEnded :: (Serialize a, AI a) => GameState a -> Handler Html
gameEnded game = expGameH game >>= gameEndedView game -- redirect . GameEndedR

-- | Redirects to the game UI in case the game is still on.
continue :: (Serialize a, AI a) => Bool -> GameState a -> Handler Html
continue firstContact game = expGameH game >>= playView firstContact game -- redirect . PlayR firstContact

-------------------------------------------------------------------------------
-- * AI
-------------------------------------------------------------------------------

-- | Performs the AI actions.
performAI :: (Serialize a, AI a) => GameState a -> Handler Html
performAI game = do
  (result, game') <- liftIO $ runStateT aiTurn game
  case result of
    Over  -> gameEnded game'
    Next  -> continue True game'
    Again -> error "impossible. `Again` is handled by aiTurn"

-- | Determines whether the countdown should already be shown.
-- Should be shown when at most countdownTurns turns remain.
showCountdown :: GameState a -> Bool
showCountdown game = remTurns <= cdTurns where
  remTurns = remainingTurns game 
  cdTurns = rulesCountdownTurns . gameRules $ game
