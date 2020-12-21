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
    SpecialDiagram
  , SpecialBackend(..)
  , ColorStyle
  , ColorStyle'(..)
  , NumericType
  )

import StringSymbols(sourceCodeDiagramLabel)

-- class SpecialBackend b => TextBoxDiagram b where
--   textSizeDiagram :: String -> SpecialDiagram b

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

textSizeDiagram :: forall b. SpecialBackend b => String -> SpecialDiagram b
textSizeDiagram t = invisibleRect where
  n = (fromIntegral . length) t
  textHeight = letterHeight
  textWidth =  n * textBoxFontSize *  monoLetterWidthToHeight (mempty :: SpecialDiagram b)
  invisibleRect =  opacity transparentAlpha $ rect textWidth textHeight

-- END Text helper functions

multilineComment :: SpecialBackend b 
  => ColorStyle -> String -> SpecialDiagram b
multilineComment colorStyle = multilineComment' (textBoxTextC colorStyle)

multilineComment' :: SpecialBackend b =>
  Colour Double -> String -> SpecialDiagram b
multilineComment' textColor t = textDia where
  textLines = lines t
  textAreas = map (coloredCommentBox textColor) textLines
  textsAlignd = map alignL textAreas
  textDia = vcat textsAlignd

coloredCommentBox :: SpecialBackend b =>
  Colour Double -> String -> SpecialDiagram b
coloredCommentBox textColor t = coloredTextBox' textColor objectWithSize textDiagram where
    textDiagram = alignedText onLeftSide inTheMiddle t
    onLeftSide = 0
    inTheMiddle =  0.5
    objectWithSize = alignL $ textSizeDiagram t

coloredTextBox ::   SpecialBackend b =>
  Colour Double -> String -> SpecialDiagram b
coloredTextBox textColor t = coloredTextBox' textColor objectWithSize textDiagram where 
  textDiagram = text t -- dont have size
  objectWithSize = textSizeDiagram t

coloredTextBox' :: SpecialBackend b =>
  Colour Double -> SpecialDiagram b -> SpecialDiagram b -> SpecialDiagram b
coloredTextBox' textColor objectWithSize textDiagram = objectWithSize <> textLabel where
  textLabel =
    fontSize
    (local textBoxFontSize)
    (font textFont $ fillColor textColor  textDiagram)


sourceCodeDiagram :: SpecialBackend b
  => String -> ColorStyle -> SpecialDiagram b
sourceCodeDiagram s colorStyle = label === sourceCode  ||| padding where
  sourceCode = multilineComment colorStyle s
  label = multilineComment colorStyle sourceCodeDiagramLabel
  padding = strut $ 10 * unitX  