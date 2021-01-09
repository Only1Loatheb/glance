{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
module TextBox
  ( coloredTextBox
  , multilineComment
  , letterHeight
  , transparentAlpha
  , sourceCodeDiagram
  )
where

import Diagrams.Prelude hiding ((&), (#), Name)

import Types(
    Drawing
  , DrawingBackend(..)
  , ColorStyle
  , ColorStyle'(..)
  , NumericType
  )

import StringSymbols(sourceCodeDiagramLabel)

-- class DrawingBackend b => TextBoxDiagram b where
--   textSizeDiagram :: String -> Drawing b

-- Text constants --
textBoxFontSize :: (Num a) => a
textBoxFontSize = 12

letterHeight :: NumericType
letterHeight = textBoxFontSize * textBoxHeightFactor

textBoxHeightFactor :: NumericType
textBoxHeightFactor = 1.4

textFont :: String
textFont = "monospace"

-- BEGIN Text helper functions --

-- | Given the number of letters in a textbox string, make a rectangle that will
-- enclose the text box. Since the normal SVG text has no size, some hackery is
-- needed to determine the size of the text's bounding box.
-- textSizeDiagram :: => Int -> t

transparentAlpha :: NumericType
transparentAlpha = 0.0

textSizeDiagram :: forall b. DrawingBackend b => String -> Drawing b
textSizeDiagram t = invisibleRect where
  n = (fromIntegral . length) t
  textHeight = letterHeight
  textWidth =  n * textBoxFontSize *  monoLetterWidthToHeight (mempty :: Drawing b)
  invisibleRect =  opacity transparentAlpha $ rect textWidth textHeight

-- END Text helper functions

multilineComment :: DrawingBackend b 
  => ColorStyle -> String -> Drawing b
multilineComment colorStyle = multilineComment' (textBoxTextC colorStyle)

multilineComment' :: DrawingBackend b =>
  Colour Double -> String -> Drawing b
multilineComment' textColor t = textDia where
  textLines = lines t
  textAreas = map (coloredCommentBox textColor) textLines
  textsAlignd = map alignL textAreas
  textDia = vcat textsAlignd

coloredCommentBox :: DrawingBackend b =>
  Colour Double -> String -> Drawing b
coloredCommentBox textColor t = coloredTextBox' textColor objectWithSize textDiagram where
    textDiagram = alignedText onLeftSide inTheMiddle t
    onLeftSide = 0
    inTheMiddle =  0.5
    objectWithSize = alignL $ textSizeDiagram t

coloredTextBox ::   DrawingBackend b =>
  Colour Double -> String -> Drawing b
coloredTextBox textColor t = coloredTextBox' textColor objectWithSize textDiagram where 
  textDiagram = text t -- dont have size
  objectWithSize = textSizeDiagram t

coloredTextBox' :: DrawingBackend b =>
  Colour Double -> Drawing b -> Drawing b -> Drawing b
coloredTextBox' textColor objectWithSize textDiagram = objectWithSize <> textLabel where
  textLabel =
    fontSize
    (local textBoxFontSize)
    (font textFont $ fillColor textColor  textDiagram)


sourceCodeDiagram :: DrawingBackend b
  => String -> ColorStyle -> Drawing b
sourceCodeDiagram s colorStyle = label === sourceCode  ||| padding where
  sourceCode = multilineComment colorStyle s
  label = multilineComment colorStyle sourceCodeDiagramLabel
  padding = strut $ 10 * unitX  