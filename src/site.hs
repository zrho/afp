--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid
import           Control.Monad
import           Hakyll


--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match (fromList pages) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= return . fmap demoteHeaders
      >>= return . fmap demoteHeaders
      >>= embedPage defaultContext

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------

embedPage :: Context String -> Item String -> Compiler (Item String)
embedPage ctx =
      loadAndApplyTemplate "templates/default.html" ctx
  >=> relativizeUrls

--------------------------------------------------------------------------------

pages :: [Identifier]
pages = [ "index.md" ]