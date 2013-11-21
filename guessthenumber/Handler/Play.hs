{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Play (getPlayR, postPlayR) where

import Import
import Data.Text
import Logic.GameState
import Handler.Helper

getPlayR :: ExtGameState -> Handler Html
getPlayR gameExt = impGame gameExt >>= maybe (redirect HomeR) (play Nothing)

postPlayR :: ExtGameState -> Handler Html
postPlayR gameExt = do
  game        <- impGame gameExt
  ((r, _), _) <- runFormPost playForm
  liftIO $ print r
  let
    num = case r of
      FormSuccess n -> Just n
      _             -> Nothing
  maybe (redirect HomeR) (play num) game
