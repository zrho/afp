----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Replay
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Handler for displaying a replay of the game.

module Handler.Replay
  ( getReplayR
  ) where

import Import
import Prelude (last)
import qualified Data.Map (lookup)
import Control.Monad.State
import Logic.Game
import Logic.GameExt
import Logic.Render
import Handler.Util

getReplayR :: GameStateExt -> Handler Html
getReplayR gameE = withGame gameE $ \game -> defaultLayout $ do
  messageRender <- getMessageRender -- needed for i18n in julius
  let rules       = gameRules game
      history     = zip [0 :: Int ..] $ reconstructHistory game
      humanGrids  = map (renderHumanGrid rules) history
      aiGrids     = map (renderAIGrid rules) history
      numSteps    = length history
  setNormalTitle
  addScript $ StaticR js_jquery_js
  $(widgetFile "replay") where
    renderHumanGrid rules (time, (humansState, aisState)) = (time, renderSvgHtml $
      renderPlayerGrid (playerFleet humansState) (playerShots aisState) ActionFire rules time)
    renderAIGrid rules (time, (humansState, aisState)) = renderHumanGrid rules (time, (aisState, humansState))

-- | Generate all the player states leading up to the current game situation.
-- Given a game state, this function generates a list of pairs of PlayerStates
-- (one for the human player, one for the AI player) which are "snapshots" of
-- previous game states. A snapshot is taken whenever `turnNumber` increases.
reconstructHistory :: GameState a -> [(PlayerState, PlayerState)]
reconstructHistory g = evalState (reconstructAndCheck g) initialGameState where
  initialGameState = GameState
    { currentPlayer  = humanPlayer
    , otherPlayer    = aiPlayer
    , aiState        = undefined
    , gameRules      = gameRules g
    , expectedAction = ActionFire
    , turnNumber     = 0
    }
  humanPlayer = let s = humanPlayerState g
                in s { playerShots = []
                     , playerFleet = reconstructOriginalFleet (playerMoves s) (playerFleet s)
                     , playerMoves = []
                     }
  aiPlayer    = let s = aiPlayerState g
                in s { playerShots = []
                     , playerFleet = reconstructOriginalFleet (playerMoves s) (playerFleet s)
                     , playerMoves = []
                     }

-- | Reconstruct game history given the current game state and check for consistency.
reconstructAndCheck :: MonadState (GameState a) m =>GameState a -> m [(PlayerState, PlayerState)]
reconstructAndCheck g = do
  let hsExpected = humanPlayerState g
      asExpected = aiPlayerState g
  history <- reconstruct -- player shots are reversed as they are originally sorted from most recent to oldest
    (reverse . playerShots $ humanPlayerState g, reverse . playerMoves $ humanPlayerState g)
    (reverse . playerShots $ aiPlayerState g, reverse . playerMoves $ aiPlayerState g)
  let (hs, as) = last history
  if as /= asExpected || hs /= hsExpected
    then error "Replay preparation: Game reconstruction unsuccessful. This shouldn't happen!"
    else return history

-- | Reconstruct the game history given the player's actions.
reconstruct :: MonadState (GameState a) m
   => (TrackingList, [ShipMove]) -- ^ current player's actions
   -> (TrackingList, [ShipMove]) -- ^ other players's actions
   -> m [(PlayerState, PlayerState)] -- ^ list of pairs of PlayerStates for every previous game situation
reconstruct ([], _) _ = do
  humansState <- gets humanPlayerState
  aisState    <- gets aiPlayerState
  return [(humansState, aisState)]
reconstruct (fstPShots, fstPMoves) sndPState = do
  humansState <- gets humanPlayerState
  aisState    <- gets aiPlayerState
  fstPShots'  <- execShots fstPShots
  fstPMoves'  <- execMove  fstPMoves
  switchRoles
  states      <- reconstruct sndPState (fstPShots', fstPMoves')
  return $ (humansState, aisState) : states

execShots :: MonadState (GameState a) m => [Shot] -> m [Shot]
execShots []           = return []
execShots (shot:shots) = do
  time <- gets turnNumber
  if shotTime shot == time
    then
      fireAt (shotPos shot)
        >>= executeShot
        >>  execShots shots
    else return $ shot:shots

execMove :: MonadState (GameState a) m => [ShipMove] -> m [ShipMove]
execMove []           = return []
execMove (move:moves) = do
  time <- gets turnNumber
  case move of
    ShipMove mId mDir mTime | time == mTime
      -> executeMove (Just (mId, mDir)) >> return moves
    _ -> return $ move:moves

-- | Reconstruct the original fleet given the moves performed.
reconstructOriginalFleet :: [ShipMove] -> Fleet -> Fleet
-- The order is crucial here!
-- The ships must be unsunk first, otherwise they might not be movable.
-- (cf. `isMovable`)
reconstructOriginalFleet moves = undoMoves moves . unsinkFleet

unsinkFleet :: Fleet -> Fleet
unsinkFleet = fmap unsinkShip where
  unsinkShip s = s { shipDamage = fmap (const False) $ shipDamage s }

undoMoves :: [ShipMove] -> Fleet -> Fleet
undoMoves ms fleet = foldl undo fleet ms where
  undo f m = case Data.Map.lookup (shipMoveID m) f of
    Just ship -> moveShip ship (reverseDirection $ shipMoveDirection m) f
    Nothing   -> error "Replay preparation: Irreversible ship movement. This shouldn't happen!"

reverseDirection :: Movement -> Movement
reverseDirection m = case m of
  Forward  -> Backward
  Backward -> Forward
