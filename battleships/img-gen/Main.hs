module Main where

import           Logic.Render
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy as BSL
import           Diagrams.Prelude
import           Diagrams.Backend.SVG
import           Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import           System.Environment
import           System.FilePath
import           System.IO


data ImgGenOpt = ImgGenOpt
  { optOutputDir :: FilePath
  } deriving (Show)

defaultOptions = ImgGenOpt
  { optOutputDir = "static"</>"img"
  }

main :: IO ()
main = do
  putStrLn "generating legend icons..."
  flip runReaderT defaultOptions $ forM_ [minBound..maxBound] writeIconFile

writeIconFile :: LegendIcon -> ReaderT ImgGenOpt IO ()
writeIconFile ico = do
  fileName <- liftM2 (</>) (asks optOutputDir) (iconFileName ico)
  liftIO $ do 
    BSL.writeFile fileName (renderIcon ico)
    putStrLn $ concat ["'", fileName, "' written"]

renderIcon :: LegendIcon -> BSL.ByteString
renderIcon = renderSvg
#if MIN_VERSION_diagrams_svg(0,8,0)
  . renderDia SVG (SVGOptions Absolute Nothing)
#else
  . renderDia SVG (SVGOptions Absolute)
#endif
  . renderLegend

iconFileName :: (MonadReader ImgGenOpt m) => LegendIcon -> m FilePath
iconFileName ico = return $ show ico ++ ".svg"

