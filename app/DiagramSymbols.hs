{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DiagramSymbols(
  defaultLineWidth
  , symbolSize
  , boxPadding
  , portSeparationSize
  , lambdaRegionPadding
  , defaultOpacity
  , defaultShadowOpacity
  , arrowLineWidth
  , arrowShadowWidth
  , inputPortSymbol
  , resultPortSymbol
  , caseVarSymbol
  , inNoteFrame
  , inCaseDecisionFrame
  , inDecisionFrame
  , inFrame
  , memptyWithPosition
  , inItemFrame
  , listDots
  , listCompPipe
  , listLitDelimiterDia
  ) where

import Diagrams.Prelude hiding ((&), (#), Name)
import Diagrams.TwoD.Combinators(strutR2)

import DrawingColors(colorScheme, ColorStyle(..))
import Types(
  SpecialDiagram
  , SpecialBackend
  )

import TextBox (
  coloredTextBox
  , letterHeight
  )

import StringSymbols(
  listDotsStr
  )

-- CONSTANTS --
defaultLineWidth :: (Fractional a) => a
defaultLineWidth = 0.15 * letterHeight

symbolSize :: (Fractional a) => a
symbolSize = 0.5 * letterHeight

boxPadding :: Fractional a => a
boxPadding = 2 * defaultLineWidth

portSeparationSize :: (Fractional a) => a
portSeparationSize = 0.3 * letterHeight

lambdaRegionPadding :: (Fractional a) => a
lambdaRegionPadding = 2.4 * letterHeight

defaultOpacity :: (Fractional a) => a
defaultOpacity = 0.4

defaultShadowOpacity :: (Fractional a) => a
defaultShadowOpacity = 0.6

arrowLineWidth :: Fractional a => a
arrowLineWidth = defaultLineWidth

arrowShadowWidth :: Fractional a => a
arrowShadowWidth = 1.9 * arrowLineWidth


-- BEGIN diagram basic symbols --
inputPortSymbol :: SpecialBackend b n => SpecialDiagram b n
inputPortSymbol = memptyWithPosition 

resultPortSymbol :: SpecialBackend b n
  => SpecialDiagram b n
resultPortSymbol = memptyWithPosition

caseVarSymbol :: SpecialBackend b n
  => Colour Double
  ->  SpecialDiagram b n
caseVarSymbol color = alignB coloredSymbol  where
    symbol = vrule (2 * symbolSize)
    coloredSymbol
      = lwG defaultLineWidth $ lc color (strokeLine symbol)

inNoteFrame :: SpecialBackend b n
  => Colour Double
  -> SpecialDiagram b n
  -> SpecialDiagram b n
inNoteFrame borderColor diagram
  = centerXY diagram <> coloredFrame where
  
    boxHeight = height diagram
    boxWidth = width diagram
    cornerSize = letterHeight / 2
    notCornerHeight = boxHeight - cornerSize 
    frameWidth = boxWidth + 2 * cornerSize
  
    offsets = [
      frameWidth *^ unitX
      , notCornerHeight *^ unitY
      , cornerSize *^ (-unitX + unitY)
      , cornerSize *^ (-unitY)
      , cornerSize *^ unitX
      , cornerSize *^ (-unitX + unitY)
      , (boxWidth + cornerSize) *^ (-unitX)
      , boxHeight *^ (-unitY)
      ]
  
    decisionFrame = centerXY $ strokeLoop $  closeLine $ fromOffsets offsets
  
    coloredFrame = lwG (defaultLineWidth/2) $  lc borderColor decisionFrame

inCaseDecisionFrame :: SpecialBackend b n
  => SpecialDiagram b n -> SpecialDiagram b n
inCaseDecisionFrame = inDecisionFrame (caseRhsC colorScheme)

inDecisionFrame :: SpecialBackend b n
  => Colour Double
  -> SpecialDiagram b n
  -> SpecialDiagram b n
inDecisionFrame borderColor diagram
  = centerXY diagram <> coloredFrame where
    boxHeight = boxPadding + max (height diagram) letterHeight
    halfBoxHeight = boxHeight / 2
    boxWidth = boxPadding + width diagram

    topRightOffsets = [
      halfBoxHeight *^ (unitY+unitX)
      , halfBoxHeight *^ (unitY-unitX)
      , boxWidth *^ (-unitX)
      ]
    bottomLeftOffsets = map negate topRightOffsets
    offsets = topRightOffsets ++ bottomLeftOffsets

    decisionFrame = centerXY $ strokeLoop $  closeLine $ fromOffsets offsets

    coloredFrame = lwG defaultLineWidth $  lc borderColor decisionFrame

inFrame :: SpecialBackend b n
  => SpecialDiagram b n
  -> Colour Double
  -> n
  -> n
  -> SpecialDiagram b n
inFrame diagram borderColor diagramWidth diagramHeight
  = centerXY diagram <> coloredArgBox where
    rectWidth = boxPadding + max diagramWidth letterHeight
    rectHeight = boxPadding + max diagramHeight letterHeight
    argBox = rect rectWidth rectHeight
    coloredArgBox = lwG defaultLineWidth $ lcA (withOpacity borderColor defaultOpacity) argBox

-- BEGIN Diagram helper functions --
memptyWithPosition :: SpecialBackend b n => SpecialDiagram b n
memptyWithPosition = strutR2 (V2 symbolSize 0)
-- | Names the diagram and puts all sub-names in the namespace of the top level

inItemFrame :: SpecialBackend b n => SpecialDiagram b n -> SpecialDiagram b n
inItemFrame itemDiagram = finalDia where
  itemDiagramAligned = alignB itemDiagram
  finalDia = beside (-unitX) (itemDiagramAligned ||| rightListItemFrame) (leftListItemFrame ||| strutX defaultLineWidth)
  leftListItemFrame = alignBR $ listCompLine $ vrule  $ (max  letterHeight $ height  itemDiagram)
  rightListItemFrame =  listDots ||| leftListItemFrame

listCompLine = lwG defaultLineWidth $ lc (listC colorScheme)

listDots :: SpecialBackend b n => SpecialDiagram b n
listDots = alignB $ coloredTextBox (listC colorScheme) listDotsStr

listCompPipe height = alignB $ centerX $ listCompLine pipe where
  line = vrule height
  pipe = hcat [line, strutX symbolSize, line]

listLitDelimiterDia :: SpecialBackend b n => String -> SpecialDiagram b n
listLitDelimiterDia str = alignB $ coloredTextBox (listC colorScheme) str