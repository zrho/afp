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
import System.IO (stdout)
#if MIN_VERSION_fast_logger(2,1,0)
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import System.Log.FastLogger (newLoggerSet, defaultBufSize)
import Network.Wai.Logger (clockDateCacher)
import Yesod.Core.Types (loggerSet, Logger (Logger))
#else
import Network.Wai.Middleware.RequestLogger
import System.Log.FastLogger (mkLogger)
#endif

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.Favicon
import Handler.Grid
import Handler.Play
import Handler.PlaceShips2
import Handler.GameEnded
import Handler.Rules

mkYesodDispatch "App" resourcesApp

makeApplication :: AppConfig DefaultEnv () -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
            then Detailed True
            else Apache FromSocket
#if MIN_VERSION_fast_logger(2,1,0)
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
#else
        , destination = Logger $ appLogger foundation
#endif
        }
    app <- toWaiAppPlain foundation
    return $ logWare app

makeFoundation :: AppConfig DefaultEnv () -> IO App
makeFoundation conf = do
#if MIN_VERSION_fast_logger(2,1,0)
    loggerSet' <- newLoggerSet defaultBufSize Nothing
    (getter, _) <- clockDateCacher

    let logger = Yesod.Core.Types.Logger loggerSet' getter
#else
    logger <- mkLogger True stdout
#endif
    s <- staticSite
    let foundation = App conf s logger
    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
