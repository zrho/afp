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
-- to the 'PlaceShipsRndR' route by the javascript code, which uses 'initShips'
-- to try and complete the player's ship placement.

module Handler.PlaceShips
  ( getPlaceShipsR
  , postPlaceShipsR
  , postPlaceShipsRndR
  ) where

import Import
import Data.Aeson (encode, decode)
import Data.Maybe
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Logic.Game
import Logic.GameExt (DefaultAI)
import Logic.AIUtil
import Handler.Util
import Handler.Play

-------------------------------------------------------------------------------
-- * Handler
-------------------------------------------------------------------------------

-- | Displays a client side UI for placing the player's fleet.
getPlaceShipsR :: Rules -> Handler Html
getPlaceShipsR rules = defaultLayout $ do
  setNormalTitle
  messageRender <- getMessageRender
  addScript $ StaticR js_jquery_js
  addScript $ StaticR js_json2_js
  addScript $ StaticR js_map_js
  $(widgetFile "board")
  $(widgetFile "placeships")

-- | Validates the player's fleet; if it's correct, the game is started.
postPlaceShipsR :: Rules -> Handler Html
postPlaceShipsR rules = do
  ships <- getPostedFleet
  case ships of
    Nothing             -> redirect $ PlaceShipsR rules
    Just fleetPlacement -> startGame rules fleetPlacement

-- | Starts a game, given the placement of the player's fleet.
startGame :: Rules -> FleetPlacement -> Handler Html
startGame rules@Rules{..} fleetPlacement = do
  let rules' = rules { rulesDevMode = development && rulesDevMode }
  game  <- liftIO (newGame rules' fleetPlacement HumanPlayer :: IO (GameState DefaultAI))
  expGameH game >>= playView game -- redirect . PlayR

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
  fleet' <- liftIO $ initShips fleet
  return $ jsonFleet $ fromMaybe [] fleet'

-------------------------------------------------------------------------------
-- * JSON Fleet Import/Export
-------------------------------------------------------------------------------

getPostedFleet :: Handler (Maybe [ShipShape])
getPostedFleet = do
  jsonStr <- runInputPost $ ireq textField "fleetData"
  return $ decode (BL.fromChunks [TE.encodeUtf8 jsonStr])

jsonFleet :: FleetPlacement -> TypedContent
jsonFleet
  = TypedContent typeJson
  . toContent
  . encode
