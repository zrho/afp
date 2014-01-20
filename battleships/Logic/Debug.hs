module Logic.Debug
  ( debugShow
  , debugId
  , debug
  , debugShowM
  , debugM
  , debug'
  ) where

import Prelude
import Settings.Development (development)

import Debug.Trace

debugShow :: Show a => a -> b -> b
debugShow = if development then traceShow else flip const

debug :: String -> a -> a
debug = if development then trace else flip const

debug' :: (a -> String) -> a -> a
debug' f x = debug (f x) x

debugId :: Show a => a -> a
debugId x = debugShow x x

debugShowM :: (Show a, Monad m) => a -> m ()
debugShowM = flip debugShow (return ())

debugM :: (Monad m) => String -> m ()
debugM = flip debug (return ())
