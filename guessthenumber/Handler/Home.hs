{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Home (getHomeR, postHomeR) where

import Import
import Data.Text

newtype Guess = Guess { get :: Int }

type Range = (Int, Int)

guessAForm :: Range -> AForm Handler Guess
guessAForm (low, high) = Guess <$> areq guessField "Your guess" Nothing where
  guessField = checkBool (\n -> low <= n && n <= high) errorMessage intField
  errorMessage = pack $ "Your number must be in the range from "
                 ++ show low ++ " to " ++ show high ++ "!"

guessForm :: Html -> MForm Handler (FormResult Guess, Widget)
guessForm = renderDivs $ guessAForm (0, 100)

getHomeR :: Handler Html
getHomeR = do
  (formWidget, enctype) <- generateFormPost guessForm
  defaultLayout $ do
    setTitle "Guess the number!"
    [whamlet|
      <form method=post action=@{HomeR} enctype=#{enctype}>
        ^{formWidget}
        <button>_{MsgGuessButton}
    |]

postHomeR :: Handler Html
postHomeR = do
  ((result, _), _) <- runFormPost guessForm
  defaultLayout $ do
    setTitle "Guess the number!"
    case result of
      FormSuccess (Guess userGuess) ->
        [whamlet|
          <p>
            Your input: #{show userGuess}
        |]
      FormFailure msgs ->
        [whamlet|
          <p>
            _{MsgInputError}#{toHtml $ mconcat msgs}
        |]
             
