{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TextBox
  ( bindTextBox
  , defaultLineWidth
  , coloredTextBox
  , transformCorrectedTextBox --} remove one
  , transformableBindTextBox -- }
  , multilineComment
  )
where

import Diagrams.Prelude hiding ((&), (#), Name)

import qualified Control.Arrow as Arrow
import Data.Either(partitionEithers)
import qualified Data.IntMap as IM
import Data.List(find)
import Data.Maybe(listToMaybe, isJust, fromJust, mapMaybe)
import Data.Typeable(Typeable)

import Constants(pattern InputPortConst, pattern ResultPortConst)
import DrawingColors(colorScheme, ColorStyle(..))
import Types(Icon(..), SpecialQDiagram, SpecialBackend, SpecialNum
            , NodeName(..), Port(..), LikeApplyFlavor(..),
            SyntaxNode(..), NamedIcon, Labeled(..), IconInfo
            , Named(..)
            ,TransformParams(..),TransformableDia)

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
rectForText :: (InSpace V2 n t, TrailLike t) => Int -> t
rectForText n = rect textWidth textHeight
  where
    textHeight = letterHeight
    textWidth = (fromIntegral n * letterWidth) + sidePadding
    sidePadding = textBoxFontSize * 0.3

-- END Text helper functions

commentTextArea :: SpecialBackend b n =>
  Colour Double -> String -> SpecialQDiagram b n
commentTextArea textColor t =
  alignL $ fontSize
  (local textBoxFontSize)
  (font textFont $ fc textColor $ topLeftText t)
  <>  alignTL (lw none $ rectForText (length t))

multilineComment :: SpecialBackend b n =>
  Colour Double
  -> AlphaColour Double -> String -> SpecialQDiagram b n
multilineComment textColor _boxColor t = lwG (0.6 * defaultLineWidth) textDia
  where
    textLines = lines t
    textAreas = map (commentTextArea textColor) textLines
    textDia = vcat textAreas

coloredTextBox :: SpecialBackend b n =>
  Colour Double
  -> AlphaColour Double -> String -> SpecialQDiagram b n
coloredTextBox textColor boxColor t
  = boxAroundText <> textLabel  where
    textLabel = alignT $ padOverText <> 
      fontSize
      (local textBoxFontSize)
      (font textFont $ fillColor textColor $ text t) -- dont have size
    padOverText = strutY (textBoxFontSize * textBoxHeightFactor /2)
    boxAroundText =
      lwG -- A convenient synonym for 'lineWidth (global w)'.
      (0.6 * defaultLineWidth)
      (lcA boxColor -- A synonym for lineColor, specialized to AlphaColour Double (i.e. colors with transparency)
        $ fcA (withOpacity (backgroundC colorScheme) 0) -- last param is radius of circular rounded corners 
        $ rectForText (length t))

transformCorrectedTextBox :: SpecialBackend b n =>
  String
  -> Colour Double
  -> Colour Double
  -> Bool
  -> Angle n
  -> SpecialQDiagram b n
transformCorrectedTextBox str textColor borderColor reflect angle =
  rotateBy
  textBoxRotation
  (reflectIfTrue reflect (coloredTextBox textColor (opaque borderColor) str))
  where
    -- If normalizeAngle is slow, the commented out function reduceAngleRange
    -- might be faster.
    reducedAngle = normalizeAngle angle ^. turn
    textBoxRotation
      = if (reducedAngle > (1/4)) && (reducedAngle < (3/4)) then 1 / 2 else 0
    reflectIfTrue shouldReflect dia
      = if shouldReflect then reflectX dia else dia

transformableBindTextBox :: SpecialBackend b n =>
  String -> Bool -> Angle n -> SpecialQDiagram b n
transformableBindTextBox str
  = transformCorrectedTextBox
    str
    (bindTextBoxTextC colorScheme)
    (bindTextBoxC colorScheme)

bindTextBox :: SpecialBackend b n =>
  String -> SpecialQDiagram b n
bindTextBox
  = coloredTextBox (bindTextBoxTextC colorScheme)
    $ opaque (bindTextBoxC colorScheme)