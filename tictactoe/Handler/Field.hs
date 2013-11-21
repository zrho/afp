module Handler.Field (getFieldR, posInPicture, picture, makeTicTacToeField) where

import Import

import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.SVG
import Diagrams.Coordinates ((&))
import GameLogic.TicTacToe
import Data.Maybe

getFieldR :: TicTacToe -> Handler TypedContent
getFieldR
  = return
  . TypedContent typeSvg
  . toContent
  . renderDia SVG (SVGOptions Absolute Nothing)
  . picture

-------------------------------------------------------------------------
-- * Field picture

posInPicture :: TicTacToe -> (Double, Double) -> Maybe Pos
posInPicture f (px, py)
  = listToMaybe 
  $ sample (picture f)
  $ p2 (px, py)

picture :: TicTacToe -> QDiagram SVG R2 [Pos]
picture = alignTL . pad 1.01 . picture'

picture' :: TicTacToe -> QDiagram SVG R2 [Pos]
picture' f = mconcat $ fmap (\p -> boxT p $ getField f p) positions where
  boxT (x, y) p
    = box p
    # translateX x'
    # translateY y'
    # value [(x, y)] where
      x' = boxSize * (fromIntegral x - 1) -- (... - 1) for centering the origin
      y' = boxSize * (1 - fromIntegral y)

  box p = square boxSize # lw 5 <> playerSymbol p

  playerSymbol Nothing  = mempty
  playerSymbol (Just X) = stroke ((topLeft ~~ bottomRight) <> (bottomLeft ~~ topRight))
                      # lw lnWidth # lc blue
  playerSymbol (Just O) = circle (half - lnWidth)
                      # lw lnWidth # lc red
  boxSize = 100 -- size of each box
  lnWidth = 5   -- line width
  half = boxSize / 2
  topLeft = origin .+^ ((-half) & half)
  bottomRight = origin .+^ (half & (-half))
  bottomLeft = origin .+^ ((-half) & (-half))
  topRight = origin .+^ (half & half)

makeTicTacToeField interactive f =
  if interactive
    then do
      let m = 303
      let m3 = div m 3
{-
      [whamlet|
        <map name="ttt">
          <area shape="rect" coords="#{   0}, #{   0}, #{  m3}, #{  m3}" alt="" href="" />
          <area shape="rect" coords="#{  m3}, #{   0}, #{2*m3}, #{  m3}" alt="" href="" />
          <area shape="rect" coords="#{2*m3}, #{   0}, #{   m}, #{  m3}" alt="" href="" />
          <area shape="rect" coords="#{   0}, #{  m3}, #{  m3}, #{2*m3}" alt="" href="" />
          <area shape="rect" coords="#{  m3}, #{  m3}, #{2*m3}, #{2*m3}" alt="" href="" />
          <area shape="rect" coords="#{2*m3}, #{  m3}, #{   m}, #{2*m3}" alt="" href="" />
          <area shape="rect" coords="#{   0}, #{2*m3}, #{  m3}, #{   m}" alt="" href="" />
          <area shape="rect" coords="#{  m3}, #{2*m3}, #{2*m3}, #{   m}" alt="" href="" />
          <area shape="rect" coords="#{2*m3}, #{2*m3}, #{   m}, #{   m}" alt="" href="" />
      |]
-}
      [whamlet|
        <p>
          <embed src="@{FieldR f}" type="image/svg+xml" border="0" usemap="#ttt" onload="t3init(this);" />
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
    else
      [whamlet|
        <p>
          <embed src="@{FieldR f}" type="image/svg+xml" />
      |]
