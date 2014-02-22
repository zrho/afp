{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Rules (getRulesR, postRulesR) where

import Import
import Logic.Game
import Handler.Util
import Data.Maybe
import Data.Traversable

getRulesR :: Handler Html
getRulesR = renderRulePage 10 Nothing

rulesForm :: (Monad m, RenderMessage (HandlerSite m) FormMessage) 
          => FormInput m [Bool]
rulesForm = sequenceA
  [ fromMaybe False <$> iopt boolField "againWhenHit"
  , fromMaybe False <$> iopt boolField "move"
  , fromMaybe False <$> iopt boolField "devMode"
  ]

postRulesR :: Handler Html
postRulesR = do
  s <- runInputPost $ rulesForm
  let
    rules = defaultRules
      { rulesAgainWhenHit = s!!0
      , rulesMove         = s!!1
      , rulesDevMode      = s!!2
      }

  redirect $ PlaceShipsR rules

renderRulePage :: Int -> Maybe AppMessage -> Handler Html
renderRulePage fieldSize formError = defaultLayout $ do
  setNormalTitle
  $(widgetFile "rules")
