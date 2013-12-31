{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, TypeFamilies #-}
module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Default.Config
import qualified Settings
import Settings.Development (development)
import Settings (widgetFile)
import Text.Hamlet (hamletFile)
#if MIN_VERSION_fast_logger(2,1,0)
import Yesod.Core.Types (Logger)
#else
import System.Log.FastLogger (Logger)
#endif
import Control.Monad
import Web.Cookie (setCookiePath)
import Logic.GameExt

data App = App
    { settings :: AppConfig DefaultEnv ()
    , getStatic :: Static -- ^ Settings for static file serving.
    , appLogger :: Logger
    }

-- Set up i18n messages. See the messages folder.
mkMessage "App" "messages" "en"

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- REPLACE whole right-hand side by "return Nothing" if no cookies needed;
    -- (avoids some potential complications, and makes things more efficient)
    makeSessionBackend _ =
      fmap (Just . if development
                   then id
                   else customizeSessionCookies $
                        \cookie -> cookie { setCookiePath = Just "/battleships" })
        $ defaultClientSessionBackend
          (120 * 60) -- session idle timeout is 120 minutes
          (if development
           then "config/client_session_key.aes"
           else "/srv/www/vhosts/www-pg-data/battleships/client_session_key.aes")

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        requestedRoute <- getCurrentRoute
        pc <- widgetToPageContent $ do
            langs <- languages
            when (head langs == "la") (toWidget [lucius|body, input {font-variant:small-caps}|])
            $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    --addStaticContent =
    --    addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
    --  where
    --    -- Generate a unique filename based on the content itself
    --    genFileName lbs
    --        | development = "autogen-" ++ base64md5 lbs
    --        | otherwise   = base64md5 lbs

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
