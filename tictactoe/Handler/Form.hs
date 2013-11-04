{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Form (getFormR, postFormR) where

import Import

import Data.Text hiding (map)
import Control.Arrow

getFormR :: Handler Html
getFormR = do (formWidget, formEnctype) <- generateFormPost someForm
              defaultLayout $ do
                setTitle "The GET page with the form on it."
                toWidget [lucius|
                          #textfield {width: 20em;}
                                |]
                [whamlet|
                 <p>
                   This is how we can do forms:
                   <form method=post action=@{FormR} enctype=#{formEnctype}>
                    <table>
                     ^{formWidget}
                    <p><input type="submit">
                        |]  

someForm :: Form (String, Int, Text)
someForm = renderTable $ (,,)
            <$> areq (radioFieldList $ map (pack &&& id) ["option1", "option2", "option3"]) "Options: " (Just "option2")
            <*> areq (selectFieldList (map (pack . show &&& id) [2..7])) "Some number: " (Just 3)
            <*> areq textField ("Some text: " {fsId = Just "textfield"}) Nothing

postFormR :: Handler Html
postFormR = do ((FormSuccess (o,i,t),_), _) <- runFormPost someForm
               defaultLayout $ do
                 [whamlet|
                  <p>
                    You selected #{o} and number #{show i}.
                  <p>
                    And, you entered the following text: #{t}
                  <p>       
                    I'll now store it in a cookie, just in case we want to access it again later on.     
                         |]
                 setSession "your text" t
                 [whamlet|
                  <p>
                    Done that. <a href=@{SkippingR 10}>Let's move on.
                         |]
