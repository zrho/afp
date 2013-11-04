{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, MultiParamTypeClasses #-}
module Main where
import Yesod
import TicTacToe
import Search
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Text
import Data.Maybe
import Data.AffineSpace.Point
import qualified Data.Text as T

-------------------------------------------------------------------------
-- * Yesod Configuration

main :: IO ()
main = warp 3000 T3App

data T3App = T3App

instance Yesod T3App

instance RenderMessage T3App FormMessage where
    renderMessage _ _ = defaultFormMessage

-------------------------------------------------------------------------
-- * Routes

mkYesod "T3App" [parseRoutes|
/#TicTacToe GameR GET POST
/field/#TicTacToe FieldR GET
|]

getGameR :: TicTacToe -> Handler Html
getGameR f = defaultLayout $ do
  [whamlet|
    <embed src="@{FieldR f}" type="image/svg+xml" onload="t3init(this);" />
  |]
  toWidgetBody [julius|

    function t3init(svg) {
      svg.getSVGDocument().onclick = t3click;
    }

    function t3click(event) {
      var form = document.createElement('form');
      form.setAttribute('method','post');
      form.setAttribute('action','@{GameR f}');
      var hiddenField = document.createElement('input');
      hiddenField.setAttribute('type','hidden');
      hiddenField.setAttribute('name','X');
      hiddenField.setAttribute('value',event.clientX);
      form.appendChild(hiddenField);
      var hiddenField=document.createElement('input');
      hiddenField.setAttribute('type','hidden');
      hiddenField.setAttribute('name','Y');
      hiddenField.setAttribute('value',-event.clientY);
      form.appendChild(hiddenField);
      document.body.appendChild(form);
      form.submit();
    }
  |]

postGameR :: TicTacToe -> Handler Html
postGameR f = do
  (px, py) <- runInputPost positionForm
  lift $ print (px, py)
  case posInPicture f (px, py) of
    Just pos -> getGameR $ play f pos
    Nothing  -> getGameR f

positionForm :: FormInput Handler (Double, Double)
positionForm = (,) <$> ireq doubleField "X" <*> ireq doubleField "Y"

getFieldR :: TicTacToe -> Handler TypedContent
getFieldR
  = return
  . TypedContent typeSvg
  . toContent
  . renderDia SVG (SVGOptions Absolute Nothing)
  . picture

-------------------------------------------------------------------------
-- ** Routes Helper

instance PathPiece TicTacToe where
  toPathPiece (TicTacToe fx fo) = T.pack $ show (fx, fo)
  fromPathPiece s =
    case reads $ T.unpack s of
      -- todo: validate
      ((fx, fo), "") : _ -> Just $ TicTacToe fx fo
      _                  -> Nothing

-------------------------------------------------------------------------
-- * Game Logic

play :: TicTacToe -> Pos -> TicTacToe
play field pos = case getField field pos of
  Just _  -> field
  Nothing -> maybe field' id $ nextDraw field' where
    field' = setField field X pos

nextDraw :: TicTacToe -> Maybe TicTacToe
nextDraw
  = fmap (\(Node x _) -> x)
  . selectMaxAB
  . prune 7
  . unfoldTree moves

-------------------------------------------------------------------------
-- * Field picture

posInPicture :: TicTacToe -> (Double, Double) -> Maybe Pos
posInPicture f (px, py)
  = listToMaybe 
  $ sample (picture f)
  $ p2 (px, py)

picture :: TicTacToe -> QDiagram SVG R2 [Pos]
picture = alignTL . scale 30 . picture'

picture' :: TicTacToe -> QDiagram SVG R2 [Pos]
picture' f = mconcat $ fmap (\p -> boxT p $ getField f p) positions where
  boxT (x, y) p
    = box p
    # translateX x'
    # translateY y'
    # value [(x, y)] where
      x' = fromIntegral x
      y' = 3 - fromIntegral y
  box p = square 1 # lw 0.2 <> player p
  player Nothing  = mempty
  player (Just O) = text "O" # translateY (-0.1)
  player (Just X) = text "X" # translateY (-0.1)