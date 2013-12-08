import qualified Data.ByteString as BS
import Codec.Crypto.SimpleAES

-- This program writes a valid key to the file keyFile.aes
main = randomKey >>= BS.writeFile "keyFile.aes"
