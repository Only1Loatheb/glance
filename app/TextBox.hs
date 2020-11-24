{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TextBox
  ( coloredTextBox
  , multilineComment
  , letterHeight
  , transparentAlpha
  , sourceCodeDiagram
  )
where

import Diagrams.Prelude hiding ((&), (#), Name)

import Types(SpecialDiagram, SpecialBackend)

import DrawingColors(ColorStyle(..))

import StringSymbols(sourceCodeDiagramLabel)

-- Text constants --
textBoxFontSize :: (Num a) => a
textBoxFontSize = 12

letterWidth :: Fractional a => a
letterWidth = textBoxFontSize * monoLetterWidthToHeightFraction

letterHeight :: Fractional a => a
letterHeight = textBoxFontSize * textBoxHeightFactor

monoLetterWidthToHeightFraction :: (Fractional a) => a
monoLetterWidthToHeightFraction = 0.72

textBoxHeightFactor :: (Fractional a) => a
textBoxHeightFactor = 1.4

sidePadding :: Fractional a => a
sidePadding = letterWidth * 0.0

textFont :: String
textFont = "monospace"

-- BEGIN Text helper functions --

-- This may be a faster implementation of normalizeAngle
--Get the decimal part of a float
-- reduceAngleRange :: SpecialNum a => a -> a
-- reduceAngleRange x = x - fromInteger (floor x)

-- | Given the number of letters in a textbox string, make a rectangle that will
-- enclose the text box. Since the normal SVG text has no size, some hackery is
-- needed to determine the size of the text's bounding box.
-- textSizeDiagram ::  => Int -> t

transparentAlpha :: Fractional a => a
transparentAlpha = 0.0

textSizeDiagram :: SpecialBackend b n 
  => String -> SpecialDiagram b n
textSizeDiagram t = invisibleRect
  where
    n = length t
    textHeight = letterHeight
    textWidth = (fromIntegral n * letterWidth) + sidePadding
    invisibleRect =  opacity transparentAlpha $ rect textWidth textHeight

-- END Text helper functions

multilineComment :: SpecialBackend b n 
  => ColorStyle Double -> String -> SpecialDiagram b n
multilineComment colorStyle = multilineComment' (textBoxTextC colorStyle)

multilineComment' :: SpecialBackend b n =>
  Colour Double -> String -> SpecialDiagram b n
multilineComment' textColor t = textDia where
  textLines = lines t
  textAreas = map (coloredCommentBox textColor) textLines
  textsAlignd = map alignL textAreas
  textDia = vcat textsAlignd

coloredCommentBox :: SpecialBackend b n =>
  Colour Double -> String -> SpecialDiagram b n
coloredCommentBox textColor t = coloredTextBox' textColor objectWithSize textDiagram where
    textDiagram = alignedText onLeftSide inTheMiddle t
    onLeftSide = 0
    inTheMiddle =  0.5
    objectWithSize = alignL $ textSizeDiagram t

coloredTextBox :: SpecialBackend b n =>
  Colour Double -> String -> SpecialDiagram b n
coloredTextBox textColor t = coloredTextBox' textColor objectWithSize textDiagram where 
  textDiagram = text t -- dont have size
  objectWithSize = textSizeDiagram t

coloredTextBox' :: SpecialBackend b n =>
  Colour Double -> SpecialDiagram b n -> SpecialDiagram b n -> SpecialDiagram b n
coloredTextBox' textColor objectWithSize textDiagram = objectWithSize <> textLabel where
  textLabel =
    fontSize
    (local textBoxFontSize)
    (font textFont $ fillColor textColor  textDiagram)


sourceCodeDiagram :: SpecialBackend b n
  => String -> ColorStyle Double -> SpecialDiagram b n
sourceCodeDiagram s colorStyle = label === sourceCode  ||| padding where
  sourceCode = multilineComment colorStyle s
  label = multilineComment colorStyle sourceCodeDiagramLabel
  padding = strut $ 10 * unitX  