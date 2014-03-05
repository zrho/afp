module Settings
  ( staticDir
  , widgetFile
  , widgetFileSettings
  , Extra (..)
  , parseExtra
  ) where

import Prelude
import Data.Default (def)
import Yesod.Default.Util
import Settings.Development (development)
import Text.Hamlet
import Data.Yaml
import Yesod.Default.Config
import Control.Applicative

import Language.Haskell.TH.Syntax
-- import Text.Shakespeare.Text (st)
-- import Yesod.Default.Config
-- import Data.Text (Text)

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Foundation.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Foundation.hs
-- staticRoot :: AppConfig DefaultEnv x -> Text
-- staticRoot conf = [st|#{appRoot conf}/cgi-bin/battleships/static|]

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

widgetFile :: String -> Q Exp
widgetFile = (if development then widgetFileReload
                             else widgetFileNoReload)
              widgetFileSettings

data Extra = Extra
  { extraMaxTurns :: Int
  , extraDataDir :: String
  , extraStaticDir :: String
  } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra <$> o .: "maxturns" <*> o .: "datadir" <*> o .: "staticdir"
