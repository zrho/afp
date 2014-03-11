{-# LANGUAGE CPP #-}
module Settings.Development
  ( development
  , production
  ) where

import Prelude


{-# INLINE development #-}
development :: Bool
development =
#if DEVELOPMENT
  True
#else
  False
#endif

{-# INLINE production #-}
production :: Bool
production = not development
