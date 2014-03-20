{-# LANGUAGE CPP #-}
module Settings.Development
  ( development
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
