{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TextBox
  ( defaultLineWidth
  , coloredTextBox
  , multilineComment
  , letterHeight
  )
where

import Diagrams.Prelude hiding ((&), (#), Name)
import Diagrams.TwoD.Combinators(strutR2)

import           Types  ( SpecialQDiagram
                        , SpecialBackend
                        )
{-# ANN module "HLint: ignore Use record patterns" #-}
{-# ANN module "HLint: ignore Unnecessary hiding" #-}

-- Text constants --
defaultLineWidth :: (Fractional a) => a
defaultLineWidth = 0.15

textBoxFontSize :: (Num a) => a
textBoxFontSize = 1

letterWidth :: Fractional a => a
letterWidth = textBoxFontSize * monoLetterWidthToHeightFraction

letterHeight :: Fractional a => a
letterHeight = textBoxFontSize * textBoxHeightFactor

monoLetterWidthToHeightFraction :: (Fractional a) => a
monoLetterWidthToHeightFraction = 0.61

textBoxHeightFactor :: (Fractional a) => a
textBoxHeightFactor = 1.4

sidePadding :: Fractional a => a
sidePadding = textBoxFontSize * 0.3

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

textSizeDiagram :: SpecialBackend b n 
  => String -> SpecialQDiagram b n
textSizeDiagram t = strutR2 (V2 textWidth textHeight)
  where
    n = length t
    textHeight = letterHeight
    textWidth = (fromIntegral n * letterWidth) + sidePadding

-- END Text helper functions

multilineComment :: SpecialBackend b n 
  => String -> SpecialQDiagram b n
multilineComment = multilineComment' white (opaque white)

multilineComment' :: SpecialBackend b n =>
  Colour Double
  -> AlphaColour Double -> String -> SpecialQDiagram b n
multilineComment' textColor _boxColor t = lwG (0.6 * defaultLineWidth) textDia
  where
    textLines = lines t
    textAreas = map (coloredTextBox textColor) textLines
    textDia = vcat textAreas

coloredTextBox :: SpecialBackend b n =>
  Colour Double -> String -> SpecialQDiagram b n
coloredTextBox textColor t
  = objectWithSize <> textLabel  where
    textLabel = alignT $ padOverText <> 
      fontSize
      (local textBoxFontSize)
      (font textFont $ fillColor textColor $ text t) -- dont have size
    padOverText = strutY (textBoxFontSize * textBoxHeightFactor /4)
    objectWithSize = textSizeDiagram t
