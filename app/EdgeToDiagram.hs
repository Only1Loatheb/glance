{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module EdgeToDiagram(
  getArrowShadowOpts
  , getArrowBaseOpts
  ) where

import Diagrams.Prelude hiding ((&), (#), Name)
import Data.Maybe(fromMaybe)
import Data.Typeable(Typeable)

import PortConstants(
  pattern InputPort
  , pattern ResultPort
  )

import Types(
  Icon(..)
  , DiagramIcon(..)
  , NodeName(..)
  , Port(..)
  , NamedIcon
  , Named(..)
  , NameAndPort(..)
  , NumericType
  , ColorStyle
  , ColorStyle'(..)
  )

import DiagramSymbols(
  arrowLineWidth
  , arrowShadowWidth
  , defaultShadowOpacity
  , symbolSize
  )

edgeControlVectorLen :: Fractional a => a
edgeControlVectorLen = symbolSize * 4.0

getArrowShadowOpts ::
  (NameAndPort,NameAndPort)
  -> (Point V2 NumericType, Point V2 NumericType)
  -> (Angle NumericType, Angle NumericType)
  -> (NamedIcon,NamedIcon)
  -> ColorStyle 
  -> ArrowOpts NumericType
getArrowShadowOpts 
  (_, namedPortTo)
  points angles iconPair colorStyle
  = shaftStyle %~ (lwG arrowShadowWidth .
                lcA $ withOpacity shaftColor defaultShadowOpacity)
  $ headStyle %~ fc shaftColor
  $ getArrowOpts points angles iconPair namedPortTo where
    shaftColor = backgroundC colorStyle

getArrowBaseOpts :: 
  (NameAndPort,NameAndPort)
  -> (Point V2 NumericType, Point V2 NumericType)
  -> (Angle NumericType, Angle NumericType)
  -> (NamedIcon, NamedIcon)
  -> ColorStyle
  -> ArrowOpts NumericType
getArrowBaseOpts 
  namesAndPorts@(_, namedPortTo)
  points
  angles 
  iconPair
  colorStyle
  = shaftStyle %~ (lwG arrowLineWidth {-- )-- -} . lc shaftColor) 
  $ headStyle %~ fc shaftColor
  $ getArrowOpts points angles iconPair namedPortTo where
    shaftColor = getShaftColor colorStyle namesAndPorts iconPair

getShaftColor :: ColorStyle -> (NameAndPort,NameAndPort) -> (NamedIcon, NamedIcon) -> Colour Double
getShaftColor colorStyle = getShaftColor' colorStyle edgeColors where
  edgeColors = edgeListC colorStyle

getShaftColor' :: ColorStyle -> [Colour Double]
  -> (NameAndPort,NameAndPort)-> (NamedIcon, NamedIcon) -> Colour Double
getShaftColor' colorStyle _ (Named _ ResultPort,_) (Named _ (Icon FunctionDefIcon {} _), _) = lambdaC colorStyle
getShaftColor' colorStyle _ (_, Named _ InputPort) (_, Named _ (Icon FunctionDefIcon {} _)) = lambdaC colorStyle
getShaftColor' _ edgeColors (Named (NodeName nodeNum) (Port portNum),_) (Named _ (Icon ListCompIcon {} _), _) = hashedShaftColor nodeNum portNum edgeColors
getShaftColor' _ edgeColors (_, Named (NodeName nodeNum) (Port portNum)) (_, Named _ (Icon ListCompIcon {} _)) = hashedShaftColor nodeNum (portNum + 1) edgeColors
getShaftColor' _ edgeColors (Named (NodeName nodeNum) (Port portNum),_) _ = hashedShaftColor nodeNum portNum edgeColors

hashedShaftColor :: Int -> Int -> [a] -> a
hashedShaftColor nodeNum portNum edgeColors = shaftColor where
  namePortHash = mod (portNum + (503 * nodeNum)) (length edgeColors)
  shaftColor = edgeColors !! namePortHash

getArrowOpts :: 
  (Point V2 NumericType, Point V2 NumericType)
  -> (Angle NumericType, Angle NumericType)
  -> (NamedIcon, NamedIcon)
  -> NameAndPort
  -> ArrowOpts NumericType
getArrowOpts (formPoint, toPoint) (anglesFrom,anglesTo) (_,iconTo) namedPortTo
  = arrowOptions where
    arrowOptions =
      -- arrowHead .~ noHead
      arrowHead .~ getArrowHead iconTo namedPortTo
      $ arrowTail .~ noTail
      $ arrowShaft .~ edgeSymbol formPoint toPoint anglesFrom anglesTo
      $ lengths .~ global symbolSize
      $ with

-- getArrowHead :: Icon -> 
getArrowHead :: NamedIcon -> NameAndPort -> ArrowHT NumericType
getArrowHead (Named iconName (Icon FunctionDefIcon {} _)) (Named nodeName InputPort) 
  = if nodeName == iconName then noHead else tri
getArrowHead _ _ = tri
-- https://archives.haskell.org/projects.haskell.org/diagrams/doc/arrow.html

edgeSymbol :: 
    Point V2 NumericType
  -> Point V2 NumericType
  -> Angle NumericType
  -> Angle NumericType
  -> Trail V2 NumericType
edgeSymbol formPoint toPoint angleFrom angleTo = fromSegments [bezier3 offsetToControl1 offsetToControl2 offsetToEnd] where
  offsetToEnd = toPoint .-. formPoint
  xOfTheOffset  = offsetToEnd ^. _x
  angleFromFlipped = flipIfOnOtherSide xOfTheOffset angleFrom
  angleToFlipped =   flipIfOnOtherSide xOfTheOffset angleTo
  offsetToControl1 = rotate angleFromFlipped (scale edgeControlVectorLen unitY)
  offsetToControl2 = rotate angleToFlipped (scale edgeControlVectorLen unitY) ^+^ offsetToEnd

flipIfOnOtherSide :: NumericType -> Angle NumericType -> Angle Double
flipIfOnOtherSide xOfTheOffset angle = if xOfTheOffset < 0 then  (- angle ^. turn) @@ turn else angle 
