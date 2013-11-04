{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Another (getAnotherR) where

import Import

getAnotherR :: Handler Html
getAnotherR = defaultLayout
              [whamlet|
               <p>This is another page. Who would have guessed.
               <p><a href=@{FormR}>Let's move on to forms.
                      |]
