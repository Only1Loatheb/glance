{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module EdgeToDiagram(
  getArrowShadowOpts
  , getArrowBaseOpts
  , ArrowPoints
  ) where

import Diagrams.Prelude hiding ((&), (#), Name)
import Diagrams.TwoD.Path.Metafont
import Data.Maybe(fromMaybe)
import Data.Typeable(Typeable)

import PortConstants(
  pattern InputPort
  , pattern ResultPort
  , pattern FunDefValuePort
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
  , PointType
  )

import DiagramSymbols(
  arrowLineWidth
  , arrowShadowWidth
  , defaultShadowOpacity
  , symbolSize
  )

type ArrowPoints = ((PointType, PointType), Maybe (PointType, PointType))

edgeControlVectorLen :: NumericType
edgeControlVectorLen = symbolSize * 4.0

getArrowShadowOpts ::
  (NameAndPort,NameAndPort)
  -> ArrowPoints
  -> (Angle NumericType, Angle NumericType)
  -> ColorStyle 
  -> ArrowOpts NumericType
getArrowShadowOpts 
  (_, namedPortTo)
  points angles colorStyle
  = shaftStyle %~ (lwG arrowShadowWidth .
                lcA $ withOpacity shaftColor defaultShadowOpacity)
  $ headStyle %~ fc shaftColor
  $ getArrowOpts points angles namedPortTo where
    shaftColor = backgroundC colorStyle

getArrowBaseOpts :: 
  (NameAndPort,NameAndPort)
  -> ArrowPoints
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
  $ getArrowOpts points angles namedPortTo where
    shaftColor = getShaftColor colorStyle namesAndPorts iconPair

getShaftColor :: ColorStyle -> (NameAndPort,NameAndPort) -> (NamedIcon, NamedIcon) -> Colour Double
getShaftColor colorStyle = getShaftColor' colorStyle edgeColors where
  edgeColors = edgeListC colorStyle

getShaftColor' :: ColorStyle -> [Colour Double]
  -> (NameAndPort,NameAndPort)-> (NamedIcon, NamedIcon) -> Colour Double
getShaftColor' colorStyle _ (Named _ ResultPort,_) (Named _ (Icon FunctionDefIcon {} _), _) = lambdaC colorStyle
getShaftColor' colorStyle _ (_, Named _ FunDefValuePort) (_, Named _ (Icon FunctionDefIcon {} _)) = lambdaC colorStyle
getShaftColor' _ edgeColors (Named (NodeName nodeNum) (Port portNum),_) _ = hashedShaftColor nodeNum portNum edgeColors

hashedShaftColor :: Int -> Int -> [a] -> a
hashedShaftColor nodeNum portNum edgeColors = shaftColor where
  namePortHash = mod (portNum + (503 * nodeNum)) (length edgeColors)
  shaftColor = edgeColors !! namePortHash

getArrowOpts :: 
  ArrowPoints
  -> (Angle NumericType, Angle NumericType)
  -> NameAndPort
  -> ArrowOpts NumericType
getArrowOpts points (anglesFrom,anglesTo) namedPortTo
  = arrowOptions where
    arrowOptions =
      -- arrowHead .~ noHead
      arrowHead .~ getArrowHead namedPortTo
      $ arrowTail .~ noTail
      $ arrowShaft .~ edgeSymbol points anglesFrom anglesTo
      $ lengths .~ global symbolSize
      $ with

-- getArrowHead :: Icon -> 
getArrowHead :: NameAndPort -> ArrowHT NumericType
getArrowHead (Named _ FunDefValuePort) =  noHead 
getArrowHead _ = tri
-- https://archives.haskell.org/projects.haskell.org/diagrams/doc/arrow.html

edgeSymbol :: 
  ArrowPoints
  -> Angle NumericType
  -> Angle NumericType
  -> Trail V2 NumericType
edgeSymbol ((formPoint, toPoint), middle) angleFrom angleTo = edge where
  edge = case middle of
    Just (mp1,mp2) -> metafont $ formPoint .- leaving offsetToControl1 -. mp1 .- tension 1.2 -. mp2 .- arriving (- offsetToControl2) -. endpt toPoint
    _ ->  fromSegments [bezier3 offsetToControl1 (offsetToControl2 ^+^ offsetToEnd) offsetToEnd]
    where
      offsetToEnd = toPoint .-. formPoint
      xOfTheOffset  = offsetToEnd ^. _x
      angleFromFlipped = flipIfOnOtherSide xOfTheOffset angleFrom
      angleToFlipped =   flipIfOnOtherSide xOfTheOffset angleTo
      offsetToControl1 = rotate angleFromFlipped (scale edgeControlVectorLen unitY)
      offsetToControl2 = rotate angleToFlipped (scale edgeControlVectorLen unitY) 

flipIfOnOtherSide :: NumericType -> Angle NumericType -> Angle Double
flipIfOnOtherSide xOfTheOffset angle = if xOfTheOffset < 0 then  (- angle ^. turn) @@ turn else angle 
