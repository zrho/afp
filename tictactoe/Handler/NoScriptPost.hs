{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.NoScriptPost (postNoScriptPostR) where

import Import
import Data.Maybe (isJust)
import GameLogic.TicTacToe
import GameLogic.Interaction
import Handler.Field
import Handler.Helper
import qualified Data.Text as T


postNoScriptPostR :: TicTacToe -> Handler Html
postNoScriptPostR f = do
  ((result, _), _) <- runFormPost fieldNumberForm
  case result of
    FormSuccess p -> let maybePos = ((p - 1) `mod` 3, (p - 1) `div` 3) in
      handleUserMove f (Just maybePos)
    FormFailure errors -> do
      (formWidget, enctype) <- generateFormPost fieldNumberForm
      defaultLayout $ do
        setDefaultTitle
        [whamlet|
          <p .error>
            #{mconcat errors}
        |]
        $(widgetFile "restart")
        let interactive = True
        $(widgetFile "field")
        $(widgetFile "noscript")
    FormMissing -> redirect (GameR f)
  
