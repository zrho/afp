{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Home where

import Import
import Logic.Game
import Logic.GameExt
import Logic.CleverAI
import Handler.Util

getHomeR :: Handler Html
getHomeR = do
  game  <- liftIO $ (newGame defaultRules [] HumanPlayer :: IO (GameState CleverAI))
  gameE <- expGame game
  defaultLayout $ do 
  	setNormalTitle
  	$(widgetFile "home")
