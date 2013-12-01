module Handler.Helper
( translateMessage
, setDefaultTitle
, fieldNumberForm
, handleUserMove
) where

import Import
import GameLogic.TicTacToe
import GameLogic.Interaction

translateMessage :: MonadHandler m => AppMessage -> m Text
translateMessage msg = do
  langs <- languages
  return $ renderMessage (undefined :: App) langs msg

setDefaultTitle :: Widget
setDefaultTitle = setTitleI MsgTitle


fieldNumberForm :: Html -> MForm Handler (FormResult Int, Widget)
fieldNumberForm =
  renderDivs $ fieldNumberAForm where
    fieldNumberAForm :: AForm Handler Int
    fieldNumberAForm = areq (checkBool validate MsgNoScriptFormError intField) (fieldSettingsLabel MsgNoScriptFormLabel) Nothing
    validate n = n >= 1 && n <= 9

handleUserMove :: TicTacToe -> Maybe Pos -> Handler Html
handleUserMove f maybePos = do
  let f' = case maybePos of
             Just pos -> userMove f pos
             Nothing  -> f
  

  let f'' = if ended f'
              then f'
              else computerMove f'

  if ended f''
    then redirect (OverR f'')
    else redirect (GameR f'')
