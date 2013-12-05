{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Favicon (getFaviconR) where

import Import

getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"