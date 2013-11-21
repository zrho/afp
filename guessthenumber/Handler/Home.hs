{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Home (getHomeR, postHomeR) where

import Import
import Data.Text
import Logic.GameState
import Handler.Helper
import System.Random (getStdRandom, randomR)

getHomeR :: Handler Html
getHomeR = defaultLayout $(widgetFile "home")

postHomeR :: Handler Html
postHomeR = do
  (lb, ub) <- runInputPost beginForm
  -- validate
  if lb > ub
    then redirect HomeR
    else startGame lb ub

startGame :: Int -> Int -> Handler Html
startGame lb ub = do
  num <- liftIO $ getStdRandom $ randomR (lb, ub)
  let state = GameState (lb, ub) num []
  state' <- expGame state
  redirect $ PlayR state'
             
