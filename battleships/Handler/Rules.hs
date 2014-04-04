-------------------------------------------------------------------------------
-- |
-- Module      :  Handler.Rules
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Handler for page that allows the user to customize the game's rules.

module Handler.Rules 
  ( getRulesR
  , postRulesEditedR
  ) where

import Import
import Data.Maybe
import Handler.Util
import Logic.Types

-------------------------------------------------------------------------------
-- * Handler
-------------------------------------------------------------------------------

-- | Handler for the rule configuration page.
getRulesR :: Options -> Handler Html
getRulesR defaultOptions = defaultLayout $ do
  setNormalTitle
  $(widgetFile "rules")

-- | Handler to accept the configured game rules.
postRulesEditedR :: Handler Html
postRulesEditedR = do
  rules <- runInputPost rulesForm
  redirect $ PlaceShipsR rules

-------------------------------------------------------------------------------
-- * Forms
-------------------------------------------------------------------------------

rulesForm :: FormInput Handler Options
rulesForm = Options
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

indexedDifficultyList :: [(Int, (AppMessage, DifficultyLevel))]
indexedDifficultyList = zip [1 ..] difficultyList
