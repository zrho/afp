{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, TypeFamilies #-}
module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Default.Config
import Yesod.Default.Util
import Settings.Development (development)
import Settings (widgetFile, Extra (..))
import Settings.StaticFiles
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Text.Julius (rawJS)
#if MIN_VERSION_fast_logger(2,1,0)
import Yesod.Core.Types (Logger)
#else
import System.Log.FastLogger (Logger)
#endif
import System.FilePath ((</>))
import Web.Cookie (setCookiePath)
import Logic.GameExt
import Logic.Game
import qualified Codec.Crypto.SimpleAES as AES

data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , appLogger :: Logger
    , appKey    :: AES.Key
    }

-- Set up i18n messages. See the messages folder.
mkMessage "App" "messages" "en"

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- REPLACE whole right-hand side by "return Nothing" if no cookies needed;
    -- (avoids some potential complications, and makes things more efficient)
    makeSessionBackend app = 
        fmap (Just . if development
                   then id
                   else customizeSessionCookies $
                        \cookie -> cookie { setCookiePath = Just "/battleships" })
        $ defaultClientSessionBackend
          (120 * 60) -- session idle timeout is 120 minutes
          (basePath </> "client_session_key.aes")
      where
        basePath = extraDataDir $ appExtra $ settings app


    defaultLayout widget = do
        -- master <- getYesod
        mmsg <- getMessage
        messageRender <- getMessageRender

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        requestedRoute <- getCurrentRoute
        let 
            isHome = case requestedRoute of
                Just HomeR -> True
                _          -> False
        pc <- widgetToPageContent $ $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- was BottomOfBody: Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfHeadBlocking

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    -- urlRenderOverride y (StaticR s) =
    --     Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    -- urlRenderOverride _ _ = Nothing

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent fileExt mimeType content = do
        staticContentPath <- fmap extraStaticDir getExtra
        addStaticContentExternal mini genFileName staticContentPath (StaticR . flip StaticRoute []) fileExt mimeType content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs
        mini
            | development = Right 
            | otherwise   = minifym


-- | Can be used instead of defaultLayout for simple pages such as "About".
plainLayout :: Widget -> Handler Html
plainLayout widget = do
  -- master <- getYesod
  -- mmsg <- getMessage
  pc <- widgetToPageContent widget
  giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod