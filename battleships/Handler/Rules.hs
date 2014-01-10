{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Rules (getRulesR, postRulesR) where

import Import
import Logic.Game
import Logic.GameExt
import Logic.CleverAI
import Handler.Util
import Data.Maybe

getRulesR :: Handler Html
getRulesR = renderRulePage 10 Nothing

postRulesR :: Handler Html
postRulesR = do
  s <- runInputPost $ iopt intField "fieldSize"
  let size = fromMaybe 10 s
  if size >= 10
    then do
           game  <- liftIO $ (newGame (rules size) [] HumanPlayer :: IO (GameState CleverAI))
           gameE <- expGame game
           redirect (PlaceShips2R gameE)
    else renderRulePage 10 $ Just MsgInvalidFieldSize

renderRulePage :: Int -> Maybe AppMessage -> Handler Html
renderRulePage fieldSize formError = defaultLayout $ do
  setNormalTitle
  $(widgetFile "rules")

rules :: Int -> Rules 
rules size = defaultRules { rulesSize = (size, size) }