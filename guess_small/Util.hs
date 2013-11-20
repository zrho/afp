module Util where

import Language.Haskell.TH.Syntax
import Data.Default (def)
import Yesod.Default.Util

widget :: FilePath -> Q Exp
widget = widgetFileNoReload def