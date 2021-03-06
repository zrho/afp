----------------------------------------------------------------------------
-- |
-- Module      :  Handler.PlaceShips
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Handler for a UI for placing the player's ships. The user configures the
-- fleet placement in a javascript client side UI, which then posts the fleet
-- JSON encoded to the handler.
--
-- Random completion of a fleet is implemented using a POST request
-- to the 'PlaceShipsRndR' route by the javascript code, which uses 'completeFleet'
-- to try and complete the player's ship placement.

module Handler.PlaceShips
  ( getPlaceShipsR
  , postPlaceShipsR
  , postPlaceShipsRndR
  ) where

import           Import
import           Data.Aeson (encode, decode)
import           Data.Maybe
import qualified Data.Text.Encoding as TE
import           Handler.Util
import           Handler.Play
import           Logic.AIUtil
import           Logic.Game
import           Logic.DefaultAI
import           Logic.Binary (fromStrict)
import           Logic.Types

-------------------------------------------------------------------------------
-- * Handler
-------------------------------------------------------------------------------

-- | Displays a client side UI for placing the player's fleet.
getPlaceShipsR :: Options -> Handler Html
getPlaceShipsR options = defaultLayout $ do
  setNormalTitle
  messageRender <- getMessageRender
  addScript $ StaticR js_jquery_js
  addScript $ StaticR js_json2_js
  addScript $ StaticR js_map_js
  $(widgetFile "board")
  $(widgetFile "placeships")

-- | Validates the player's fleet; if it's correct, the game is started.
postPlaceShipsR :: Options -> Handler Html
postPlaceShipsR options = do
  ships <- getPostedFleet
  case ships of
    Nothing             -> redirect $ PlaceShipsR options
    Just fleetPlacement -> startGame options fleetPlacement

-- | Starts a game, given the placement of the player's fleet.
startGame :: Options -> FleetPlacement -> Handler Html
startGame Options{..} fleetPlacement = do
  extra <- getExtra
  let
    rules = Rules
      { rulesAgainWhenHit   = againWhenHit
      , rulesMove           = move
      , rulesDifficulty     = difficulty
      , rulesMaximumTurns   = extraMaxTurns extra
      , rulesCountdownTurns = extraCountdownTurns extra
      }
  game  <- liftIO (newGame rules noviceMode (development && devMode) fleetPlacement HumanPlayer :: IO (GameState DefaultAI))
  expGameH game >>= playView True game -- redirect . PlayR True

-------------------------------------------------------------------------------
-- * Random Completion
-------------------------------------------------------------------------------

-- | POST handler for completing a player's fleet.
--
-- Accepts a posted fleet and tries to complete it; if there is a valid
-- completion, that one is sent back to the UI. Otherwise an empty fleet is
-- returned in order to indicate that no such completion exists.
postPlaceShipsRndR :: Handler TypedContent
postPlaceShipsRndR = do
  fleet  <- fmap (fromMaybe []) getPostedFleet
  fleet' <- liftIO $ completeFleet fleet
  return $ jsonFleet $ fromMaybe [] fleet'

-------------------------------------------------------------------------------
-- * JSON Fleet Import/Export
-------------------------------------------------------------------------------

-- | Parses fleet submitted by POST in a field named "fleetData".
getPostedFleet :: Handler (Maybe FleetPlacement)
getPostedFleet = do
  jsonStr <- runInputPost $ ireq textField "fleetData"
  return $ decode (fromStrict $ TE.encodeUtf8 jsonStr)

-- | Converts a FleetPlacement to JSON data
jsonFleet :: FleetPlacement -> TypedContent
jsonFleet
  = TypedContent typeJson
  . toContent
  . encode
