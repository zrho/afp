{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Data.Default (def)
import Yesod.Default.Config
import Yesod.Default.Main (defaultDevelApp)
import Network.Wai.Middleware.RequestLogger
import System.IO (stdout)
import System.Log.FastLogger (mkLogger)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Game
import Handler.Field
import Handler.Over
import Handler.Smiley
import Handler.Start
import Handler.Helper

mkYesodDispatch "App" resourcesApp

makeApplication :: AppConfig DefaultEnv () -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
            then Detailed True
            else Apache FromSocket
        , destination = Logger $ appLogger foundation
        }
    app <- toWaiAppPlain foundation
    return $ logWare app

makeFoundation :: AppConfig DefaultEnv () -> IO App
makeFoundation conf = do
    logger <- mkLogger True stdout
    let foundation = App conf logger
    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
