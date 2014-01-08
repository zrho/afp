{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Handler.Play where

import Import
import Logic.Game
import Logic.GameExt
import Handler.Util

positionForm :: FormInput Handler (Double, Double, Bool)
positionForm = (,,) <$> ireq doubleField "X" <*> ireq doubleField "Y" <*> ireq boolField "move"

getPlayR :: GameStateExt -> Handler Html
getPlayR gameE = withGame gameE $ \(GameState {..}) -> defaultLayout $ do
  setNormalTitle
  $(widgetFile "play")

postPlayR :: GameStateExt -> Handler Html
postPlayR gameE = withGame gameE $ \game -> do
  pos' <- runInputPost positionForm
  res <- case pos' of
    (x,y,m)  -> case fieldPos game (x,y) of
      Nothing  -> return $ Next game
      Just pos -> case m of 
        True  -> liftIO $ move game pos
        False -> liftIO $ turn game pos
  case res of
    Won g  -> expGame g >>= redirect . GameEndedR
    Lost g -> expGame g >>= redirect . GameEndedR
    Next g -> expGame g >>= redirect . PlayR 