{-# LANGUAGE CPP #-}
import Prelude              (IO, (>>=), return)
import Application          (makeApplication)

#if DEVELOPMENT
import qualified Network.Wai.Handler.Warp (run)
import Yesod.Default.Config (loadConfig, configSettings, DefaultEnv(Development))

main :: IO ()
main =
    loadConfig (configSettings Development)
    >>= makeApplication
    >>= Network.Wai.Handler.Warp.run 3000
#else
import qualified Network.Wai.Handler.CGI (run)
import Yesod.Default.Config (loadConfig, configSettings, DefaultEnv(Production), ConfigSettings(csFile))

main :: IO ()
main =
    loadConfig ((configSettings Production) {csFile = \_ -> return "/srv/www/vhosts/www-pg/cgi-bin/settings.yml"})
    >>= makeApplication
    >>= Network.Wai.Handler.CGI.run
#endif
