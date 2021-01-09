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
  , lambdaRegionPaddingX
  , lambdaRegionPaddingY
  , defaultOpacity
  , defaultShadowOpacity
  , arrowLineWidth
  , arrowShadowWidth
  , inputPortSymbol
  , resultPortSymbol
  , caseValSymbol
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

import Types(
  Drawing
  , DrawingBackend
  , ListLitFlavor(..)
  , NumericType
  , ColorStyle
  , ColorStyle'(..)
  )

import TextBox (
  coloredTextBox
  , letterHeight
  )

import StringSymbols(
  listDotsStr
  )

-- CONSTANTS --
defaultLineWidth :: NumericType
defaultLineWidth = 0.15 * letterHeight

symbolSize :: NumericType
symbolSize = 0.5 * letterHeight

boxPadding :: NumericType
boxPadding = 2 * defaultLineWidth

portSeparationSize :: NumericType
portSeparationSize = 0.3 * letterHeight

lambdaRegionPaddingX :: NumericType
lambdaRegionPaddingX = 1.4 * letterHeight

lambdaRegionPaddingY :: NumericType
lambdaRegionPaddingY = 2 * letterHeight

defaultOpacity :: NumericType
defaultOpacity = 0.4

defaultShadowOpacity :: NumericType
defaultShadowOpacity = 0.6

arrowLineWidth :: NumericType
arrowLineWidth = defaultLineWidth

arrowShadowWidth :: NumericType
arrowShadowWidth = 1.9 * arrowLineWidth


-- BEGIN diagram basic symbols --
inputPortSymbol :: DrawingBackend b => Drawing b
inputPortSymbol = memptyWithPosition 

resultPortSymbol :: DrawingBackend b
  => Drawing b
resultPortSymbol = memptyWithPosition

caseValSymbol :: DrawingBackend b
  => Colour Double
  ->  Drawing b
caseValSymbol color = alignB coloredSymbol  where
    symbol = vrule $ 2 * defaultLineWidth
    coloredSymbol
      = lwG defaultLineWidth $ lc color (strokeLine symbol)

inNoteFrame :: DrawingBackend b
  => Colour Double
  -> Drawing b
  -> Drawing b
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

inCaseDecisionFrame :: DrawingBackend b
  => ColorStyle -> Drawing b -> Drawing b
inCaseDecisionFrame colorStyle = inDecisionFrame (caseRhsC colorStyle)

inDecisionFrame :: DrawingBackend b
  => Colour Double
  -> Drawing b
  -> Drawing b
inDecisionFrame borderColor diagram
  = centerXY diagram <> coloredFrame where
    boxHeight = boxPadding + max (height diagram) letterHeight
    halfBoxHeight = boxHeight / 2
    boxWidth = boxPadding + width diagram

    topRightOffsets = [
        halfBoxHeight *^ unitY + symbolSize *^ unitX
      , halfBoxHeight *^ unitY - symbolSize *^ unitX
      , boxWidth *^ (-unitX)
      ]
    bottomLeftOffsets = map negate topRightOffsets
    offsets = topRightOffsets ++ bottomLeftOffsets

    decisionFrame = centerXY $ strokeLoop $  closeLine $ fromOffsets offsets

    coloredFrame = lwG defaultLineWidth $  lc borderColor decisionFrame

inFrame :: DrawingBackend b
  => Drawing b
  -> Colour Double
  -> NumericType
  -> NumericType
  -> Drawing b
inFrame diagram borderColor diagramWidth diagramHeight
  = centerXY diagram <> coloredArgBox where
    rectWidth = boxPadding + max diagramWidth letterHeight
    rectHeight = boxPadding + max diagramHeight letterHeight
    argBox = rect rectWidth rectHeight
    coloredArgBox = lwG defaultLineWidth $ lcA (withOpacity borderColor defaultOpacity) argBox

-- BEGIN Diagram helper functions --
memptyWithPosition :: DrawingBackend b => Drawing b
memptyWithPosition = strutR2 (V2 symbolSize 0)
-- | Names the diagram and puts all sub-names in the namespace of the top level

inItemFrame :: DrawingBackend b => ColorStyle -> Drawing b -> Drawing b
inItemFrame colorStyle itemDiagram = finalDia where
  itemDiagramAligned = alignB itemDiagram
  finalDia = beside (-unitX) (itemDiagramAligned ||| rightListItemFrame) (leftListItemFrame ||| strutX defaultLineWidth)
  leftListItemFrame = alignBR $ listCompLine colorStyle $ vrule $ max  letterHeight (height  itemDiagram)
  rightListItemFrame =  listDots colorStyle ||| leftListItemFrame

listCompLine :: DrawingBackend b => ColorStyle -> (Drawing b -> Drawing b)
listCompLine colorStyle = lwG defaultLineWidth $ lc (listC colorStyle)

listDots :: DrawingBackend b => ColorStyle -> Drawing b
listDots colorStyle = alignB $ coloredTextBox (listC colorStyle) listDotsStr

listCompPipe :: DrawingBackend b => ColorStyle -> NumericType -> Drawing b
listCompPipe colorStyle pipeHeight = alignB $ centerX $ listCompLine colorStyle pipe where
  line = vrule pipeHeight
  pipe = hcat [line, strutX symbolSize, line]

listLitDelimiterDia :: DrawingBackend b => ColorStyle -> ListLitFlavor -> String -> Drawing b
listLitDelimiterDia colorStyle ListFlavor str = alignB $ coloredTextBox (listC colorStyle) str
listLitDelimiterDia colorStyle TupleFlavor str = alignB $ coloredTextBox (tupleC colorStyle) str
