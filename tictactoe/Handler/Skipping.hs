{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Skipping (getSkippingR) where

import Import

getSkippingR :: Int -> Handler Html
getSkippingR i | i > 0
               = defaultLayout
                 [whamlet|
                  <p>Let's skip through a few pages.
                  <p>#{i} pages still left.
                  <p>Keep watch on the address line (and check the source file Skipping.hs for how the counting is realized).
                  <p><a href=@{SkippingR (i - 1)}>Next page.
                         |]
getSkippingR 0 = defaultLayout $ do
  [whamlet|<p>Okay, I think that's enough.|]
  Just t <- lookupSession "your text"
  [whamlet|<p>BTW, your text (stored in the cookie earlier on) was as follows: #{t}|]
