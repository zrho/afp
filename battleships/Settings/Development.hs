{-# LANGUAGE CPP #-}
module Settings.Development
  ( development
  , production
  ) where

import Prelude

development :: Bool
development =
#if DEVELOPMENT
  True
#else
  False
#endif

production :: Bool
production = not development
