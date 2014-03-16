----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Rules
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Handler for page that allows the user to customize the game's rules.

module Handler.Rules 
  ( getRulesR
  , postRulesR
  ) where

import Import
import Logic.Game
import Handler.Util
import Data.Maybe

-------------------------------------------------------------------------------
-- * Handler
-------------------------------------------------------------------------------

-- | Handler for the rule configuration page.
getRulesR :: Handler Html
getRulesR = renderRulePage Nothing

-- | Handler to accept the configured game rules.
postRulesR :: Handler Html
postRulesR = do
  rules <- runInputPost rulesForm
  redirect $ PlaceShipsR rules

-- | Displays the rule configuration page.
renderRulePage :: Maybe AppMessage -> Handler Html
renderRulePage formError = defaultLayout $ do
  setNormalTitle
  $(widgetFile "rules")

-------------------------------------------------------------------------------
-- * Forms
-------------------------------------------------------------------------------

rulesForm :: FormInput Handler PreRules
rulesForm = PreRules
  <$> (fromMaybe False <$> iopt boolField "againWhenHit")
  <*> (fromMaybe False <$> iopt boolField "move")
  <*> (fromMaybe False <$> iopt boolField "noviceMode")
  <*> (fromMaybe False <$> iopt boolField "devMode")
  <*> (fromMaybe Hard  <$> iopt (selectFieldList difficultyList) "difficulty")

difficultyList :: [(AppMessage, DifficultyLevel)]
difficultyList =
  [ (MsgInputDifficultyHard, Hard)
  , (MsgInputDifficultyMedium, Medium)
  , (MsgInputDifficultyEasy, Easy)
  ]

zippedDifficultyList :: [(Int, (AppMessage, DifficultyLevel))]
zippedDifficultyList = zip [1 ..] difficultyList
