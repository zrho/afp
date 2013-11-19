{-# LANGUAGE CPP #-}
import Prelude              (IO, (>>=), return)
import Yesod.Default.Config (loadConfig, configSettings, DefaultEnv(Development, Production), ConfigSettings(csFile))
import Application          (makeApplication)

#if DEVELOPMENT
import qualified Network.Wai.Handler.Warp (run)

main :: IO ()
main =
    loadConfig (configSettings Development)
    >>= makeApplication
    >>= Network.Wai.Handler.Warp.run 3000
#else
import qualified Network.Wai.Handler.CGI (run)

main :: IO ()
main =
    loadConfig ((configSettings Production) {csFile = \_ -> return "/srv/www/vhosts/www-pg/cgi-bin/settings.yml"})
    >>= makeApplication
    >>= Network.Wai.Handler.CGI.run
#endif
