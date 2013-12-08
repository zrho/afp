{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Play (getPlayR, postPlayR) where

import Import
import Logic.GameState
import Handler.Helper
import qualified Data.List

getPlayR :: ExtGameState -> Handler Html
getPlayR gameExt = impGame gameExt >>= maybe (redirect HomeR) display

postPlayR :: ExtGameState -> Handler Html
postPlayR gameExt = do
  game        <- impGame gameExt
  ((r, _), _) <- runFormPost playForm
  let
    num = case r of
      FormSuccess n -> Just n
      _             -> Nothing
  maybe (redirect HomeR) (updateState num) game
  

updateState :: Maybe Int -> GameState -> Handler Html
updateState (Just n) g = do
  let oldHistory = gameHistory g
      newHistory = oldHistory ++ [n]
      g' = g { gameHistory = newHistory }
  extGameState <- expGame g'
  redirect $ PlayR extGameState
updateState Nothing g = do
  gameExt <- expGame g
  redirect $ PlayR gameExt

display :: GameState -> Handler Html
display g = do
  gameExt     <- expGame g
  (form, enc) <- generateFormPost playForm
  let (ub, lb) = gameRange g
      answer   = gameAnswer g
      history  = gameHistory g
      guess    = if Data.List.null history then Nothing else Just $ Data.List.last history
  defaultLayout $ case guess of
    Just n -> if n == answer
                then $(widgetFile "win")
                else $(widgetFile "play")
    Nothing -> $(widgetFile "play")
  
