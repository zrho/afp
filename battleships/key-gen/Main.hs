module Main where
import Crypto.Random
import System.Environment
import qualified Data.ByteString as BS
import System.IO

main :: IO ()
main = do
  args <- getArgs
  entPool <- createEntropyPool
  let
    cprg    = cprgCreate entPool :: SystemRNG
    (key,_) = cprgGenerate 32 cprg
  withFile (head (args ++ ["config/key.aes"])) WriteMode $ \handle -> do
    BS.hPutStr handle key
    hFlush handle
  putStrLn "key generated"
