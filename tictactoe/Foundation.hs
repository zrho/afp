{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
module Foundation where

import Prelude
import Yesod
import Yesod.Default.Config
import Settings (widgetFile)
import Settings.Development (development)
import Text.Hamlet (hamletFile)
import System.Log.FastLogger (Logger)
import Web.Cookie (setCookiePath)

--------------------------------------------------------------------------------
-- I added the following here because I wasn't sure where to put it - Fabian
-- TODO: find good location

import GameLogic.TicTacToe
import qualified Data.Text as T

-------------------------------------------------------------------------
-- ** Routes Helper

instance PathPiece TicTacToe where
  toPathPiece (TicTacToe fx fo) = T.pack $ show (fx, fo)
  fromPathPiece s =
    case reads $ T.unpack s of
      -- TODO: validate
      ((fx, fo), "") : _ -> Just $ TicTacToe fx fo
      _                  -> Nothing
--------------------------------------------------------------------------------

data App = App
    { settings :: AppConfig DefaultEnv ()
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
    makeSessionBackend _ = return Nothing

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
