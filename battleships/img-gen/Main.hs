{-# LANGUAGE CPP #-}
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
import qualified Settings (staticDir)


data ImgGenOpt = ImgGenOpt
  { optOutputDir :: FilePath
  } deriving (Show)

defaultOptions = ImgGenOpt
  { optOutputDir = Settings.staticDir </> "img"
  }

type Dia = QDiagram SVG R2 Any

main :: IO ()
main = flip runReaderT defaultOptions $ do
  liftIO $ putStrLn "generating legend icons..."
  forM_ [minBound..maxBound :: LegendIcon] writeImageFile
  writeImageFile GridBG

-------------------------------------------------------------------------------
-- * Diagram Rendering
-------------------------------------------------------------------------------

diaToSVG :: Dia -> BSL.ByteString
diaToSVG = renderSvg
#if MIN_VERSION_diagrams_svg(0,8,0)
  . renderDia SVG (SVGOptions Absolute Nothing)
#else
  . renderDia SVG (SVGOptions Absolute)
#endif

-------------------------------------------------------------------------------
-- * Class
-------------------------------------------------------------------------------

class ImageFile a where
  imageName   :: a -> String
  imageRender :: a -> Dia

instance ImageFile LegendIcon where
  imageName   = show
  imageRender = renderLegend

-------------------------------------------------------------------------------
-- * Grid Background
-------------------------------------------------------------------------------

data GridBG = GridBG

instance ImageFile GridBG where
  imageName _   = "grid"
  imageRender _ = fmap (const $ Any True) renderGrid

-------------------------------------------------------------------------------
-- * Icons
-------------------------------------------------------------------------------

writeImageFile :: ImageFile a => a -> ReaderT ImgGenOpt IO ()
writeImageFile img = do
  fileName <- imageFileName img
  liftIO $ do 
    BSL.writeFile fileName (diaToSVG $ imageRender img)
    putStrLn $ concat ["'", fileName, "' written"]

imageFileName
  :: (MonadReader ImgGenOpt m, ImageFile a)
  => a -> m FilePath
imageFileName img = do
  dir <- asks optOutputDir
  return $ dir </> (imageName img ++ ".svg")
