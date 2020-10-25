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
  )
where

import Diagrams.Prelude hiding ((&), (#), Name)
import Diagrams.TwoD.Combinators(strutR2)

import           Types  ( SpecialDiagram
                        , SpecialBackend
                        )
{-# ANN module "HLint: ignore Use record patterns" #-}
{-# ANN module "HLint: ignore Unnecessary hiding" #-}

-- Text constants --
textBoxFontSize :: (Num a) => a
textBoxFontSize = 1

letterWidth :: Fractional a => a
letterWidth = textBoxFontSize * monoLetterWidthToHeightFraction

letterHeight :: Fractional a => a
letterHeight = textBoxFontSize * textBoxHeightFactor

monoLetterWidthToHeightFraction :: (Fractional a) => a
monoLetterWidthToHeightFraction = 0.631

textBoxHeightFactor :: (Fractional a) => a
textBoxHeightFactor = 1.4

sidePadding :: Fractional a => a
sidePadding = letterWidth * 0.0

textFont :: String
textFont = "monospace"

textCorrection :: Fractional n => P2 n
textCorrection = p2 (0,- textBoxFontSize * textBoxHeightFactor /4)
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
  => String -> SpecialDiagram b n
multilineComment = multilineComment' white

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
  textLabel = moveTo textCorrection $
    fontSize
    (local textBoxFontSize)
    (font textFont $ fillColor textColor  textDiagram)