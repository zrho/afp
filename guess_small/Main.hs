{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, MultiParamTypeClasses #-}
module Main where

import           Yesod
import           Text.Hamlet (hamletFile)
import           Text.Cassius (cassiusFile)
import           GameState
import           Util
import           Control.Applicative
import           Control.Monad
import           System.Random
import qualified Data.Text as T

-------------------------------------------------------------------------
-- * Yesod Configuration

main :: IO ()
main = warp 3000 GuessTheNumber

data GuessTheNumber = GuessTheNumber

instance Yesod GuessTheNumber where
  defaultLayout w = do
    pc <- widgetToPageContent $(widget "default")
    giveUrlRenderer $(hamletFile "templates/default-wrapper.hamlet")

instance RenderMessage GuessTheNumber FormMessage where
    renderMessage _ _ = defaultFormMessage

-------------------------------------------------------------------------
-- * Routes

mkYesod "GuessTheNumber" [parseRoutes|
/ HomeR GET POST
/play/#ExtGameState PlayR GET POST
|]

beginForm = (,)
  <$> ireq intField "lower"
  <*> ireq intField "upper"

playForm = renderDivs $ areq intField "Your guess" Nothing

getHomeR :: Handler Html
getHomeR = defaultLayout $(widget "home")

postHomeR :: Handler Html
postHomeR = do
  (lb, ub) <- runInputPost beginForm
  -- validate
  if lb > ub
    then redirect HomeR
    else startGame lb ub

startGame :: Int -> Int -> Handler Html
startGame lb ub = do
  num <- liftIO $ getStdRandom $ randomR (lb, ub)
  let state = GameState (lb, ub) num []
  state' <- expGame state
  redirect $ PlayR state'

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

play :: Maybe Int -> GameState -> Handler Html
play (Just n) g | gameAnswer g == n = defaultLayout $(widget "win")
play wrong g = do
  gameExt     <- expGame g
  (form, enc) <- generateFormPost playForm
  let (ub, lb) = gameRange g
  let answer   = gameAnswer g
  defaultLayout $(widget "play")