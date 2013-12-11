{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Handler.Play where

import Import
import Logic.Game
import Logic.GameExt
import Handler.Util

positionForm :: FormInput Handler (Double, Double)
positionForm = (,) <$> ireq doubleField "X" <*> ireq doubleField "Y"

getPlayR :: GameStateExt -> Handler Html
getPlayR gameE = withGame gameE $ \(GameState {..}) -> defaultLayout $ do 
                                                         setNormalTitle
                                                         $(widgetFile "play")

postPlayR :: GameStateExt -> Handler Html
postPlayR gameE = withGame gameE $ \game -> do
  pos' <- runInputPost positionForm
  res  <- case fieldPos game pos' of
    Nothing  -> return $ Next game
    Just pos -> liftIO $ turn game pos
  case res of
    Won g  -> expGame g >>= redirect . GameEndedR
    Lost g -> expGame g >>= redirect . GameEndedR
    Next g -> expGame g >>= redirect . PlayR 