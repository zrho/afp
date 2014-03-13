{-# LANGUAGE PackageImports #-}
import "battleships" Application (getApplicationDev)

-- Bis auf den Paketnamen muss in dieser Datei im Normalfall nichts geändert werden.

import Network.Wai.Handler.Warp (runSettings, defaultSettings, Settings(..))
import Control.Concurrent (forkIO, threadDelay)
import System.Directory (doesFileExist)
import System.Exit (exitSuccess)

setPort :: Int -> Settings -> Settings
setPort port settings = settings { settingsPort = port }

main :: IO ()
main = do
    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev
    let settings = setPort port defaultSettings
    forkIO $ runSettings settings app
    loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "yesod-devel/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess
