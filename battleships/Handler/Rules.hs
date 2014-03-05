----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Rules
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Handler for page that allows the user to customize the game's rules.

{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Rules 
  ( getRulesR
  , postRulesR
  ) where

import Import
import Logic.Game
import Handler.Util
import Data.Maybe
import Data.Traversable

-------------------------------------------------------------------------------
-- * Handler
-------------------------------------------------------------------------------

-- | Handler for the rule configuration page.
getRulesR :: Handler Html
getRulesR = renderRulePage Nothing

-- | Handler to accept the configured game rules.
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

-- | Displays the rule configuration page.
renderRulePage :: Maybe AppMessage -> Handler Html
renderRulePage formError = defaultLayout $ do
  setNormalTitle
  $(widgetFile "rules")

-------------------------------------------------------------------------------
-- * Forms
-------------------------------------------------------------------------------

rulesForm :: (Monad m, RenderMessage (HandlerSite m) FormMessage) 
          => FormInput m [Bool]
rulesForm = sequenceA
  [ fromMaybe False <$> iopt boolField "againWhenHit"
  , fromMaybe False <$> iopt boolField "move"
  , fromMaybe False <$> iopt boolField "devMode"
  ]