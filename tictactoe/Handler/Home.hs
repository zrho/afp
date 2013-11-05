{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Home (getHomeR) where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  $(widgetFile "home")
  [whamlet|
     <embed src=@{CircleR} type="image/svg+xml">
          |]
  --setMessage "This message was set on the previous page. It goes away if you reload this page."
  [whamlet|
   <p>
     <a href=@{AnotherR}>Going to another page?
          |]
