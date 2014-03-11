{-# LANGUAGE CPP #-}
import Prelude              (IO, (>>=))
import Yesod.Default.Config (loadConfig, configSettings, DefaultEnv(..), ConfigSettings(csParseExtra))
import Settings             (parseExtra)
import Application          (makeApplication)


#if DEVELOPMENT
import qualified Network.Wai.Handler.Warp (run)

main :: IO ()
main =
    loadConfig (configSettings Development) { csParseExtra = parseExtra }
    >>= makeApplication
    >>= Network.Wai.Handler.Warp.run 3000
#else
import qualified Network.Wai.Handler.FastCGI (run)

main :: IO ()
main =
    loadConfig (configSettings Production) { csParseExtra = parseExtra }
    >>= makeApplication
    >>= Network.Wai.Handler.FastCGI.run
#endif
