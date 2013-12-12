{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Home where

import Import
import Logic.Game
import Logic.GameExt
import Logic.StupidAI
import Handler.Util

getHomeR :: Handler Html
getHomeR = do
  game  <- liftIO $ (newGame rules [] :: IO (GameState StupidAI))
  gameE <- expGame game
  defaultLayout $ do 
  	setNormalTitle
  	$(widgetFile "home")

rules :: Rules 
rules = Rules (10, 10) [ 5, 4, 4, 3, 3, 3, 2, 2, 2, 2 ] 1