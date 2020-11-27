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

import DrawingColors(ColorStyle(..))

import PortConstants(
  pattern InputPortConst
  , pattern ResultPortConst
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
  -> (Maybe (Angle NumericType), Maybe (Angle NumericType))
  -> (NamedIcon,NamedIcon)
  -> ColorStyle Double 
  -> ArrowOpts NumericType
getArrowShadowOpts 
  (_, namedPortTo)
  points maybeAngles iconPair colorStyle
  = shaftStyle %~ (lwG arrowShadowWidth .
                lcA $ withOpacity shaftColor defaultShadowOpacity)
  $ headStyle %~ fc shaftColor
  $ getArrowOpts points maybeAngles iconPair namedPortTo where
    shaftColor = backgroundC colorStyle

getArrowBaseOpts :: 
  (NameAndPort,NameAndPort)
  -> (Point V2 NumericType, Point V2 NumericType)
  -> (Maybe (Angle NumericType), Maybe (Angle NumericType))
  -> (NamedIcon, NamedIcon)
  -> ColorStyle Double
  -> ArrowOpts NumericType
getArrowBaseOpts 
  namesAndPorts@(_, namedPortTo)
  points maybeAngles 
  iconPair
  colorStyle
  = shaftStyle %~ (lwG arrowLineWidth {-- )-- -} . lc shaftColor) 
  $ headStyle %~ fc shaftColor
  $ getArrowOpts points maybeAngles iconPair namedPortTo where
    shaftColor = getShaftColor colorStyle namesAndPorts iconPair

getShaftColor :: ColorStyle Double -> (NameAndPort,NameAndPort) -> (NamedIcon, NamedIcon) -> Colour Double
getShaftColor colorStyle = getShaftColor' colorStyle edgeColors where
  edgeColors = edgeListC colorStyle

getShaftColor' :: ColorStyle Double -> [Colour Double]
  -> (NameAndPort,NameAndPort)-> (NamedIcon, NamedIcon) -> Colour Double
getShaftColor' colorStyle _ (NameAndPort _ ResultPortConst,_) (Named _ (Icon FunctionDefIcon {} _), _) = lambdaC colorStyle
getShaftColor' colorStyle _ (_, NameAndPort _ InputPortConst) (_, Named _ (Icon FunctionDefIcon {} _)) = lambdaC colorStyle
getShaftColor' _ edgeColors (NameAndPort (NodeName nodeNum) (Port portNum),_) (Named _ (Icon ListCompIcon {} _), _) = hashedShaftColor nodeNum portNum edgeColors
getShaftColor' _ edgeColors (_, NameAndPort (NodeName nodeNum) (Port portNum)) (_, Named _ (Icon ListCompIcon {} _)) = hashedShaftColor nodeNum (portNum + 1) edgeColors
getShaftColor' _ edgeColors (NameAndPort (NodeName nodeNum) (Port portNum),_) _ = hashedShaftColor nodeNum portNum edgeColors

hashedShaftColor :: Int -> Int -> [a] -> a
hashedShaftColor nodeNum portNum edgeColors = shaftColor where
  namePortHash = mod (portNum + (503 * nodeNum)) (length edgeColors)
  shaftColor = edgeColors !! namePortHash

getArrowOpts :: 
  (Point V2 NumericType, Point V2 NumericType)
  -> (Maybe (Angle NumericType), Maybe (Angle NumericType))
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
      -- TODO Don't use a magic number for lengths (headLength and tailLength)
      $ lengths .~ global symbolSize
      $ with

-- getArrowHead :: Icon -> 
getArrowHead :: NamedIcon -> NameAndPort -> ArrowHT NumericType
getArrowHead (Named iconName (Icon FunctionDefIcon {} _)) (NameAndPort nodeName InputPortConst) 
  = if nodeName == iconName then noHead else tri
getArrowHead _ _ = tri
-- https://archives.haskell.org/projects.haskell.org/diagrams/doc/arrow.html

edgeSymbol :: (R1 (Diff p), Affine p, Transformable (Diff p (N t)),
                 TrailLike t, Floating (N (Diff p (N t))), Eq (N (Diff p (N t))),
                 V (Diff p (N t)) ~ V2, V t ~ Diff p)
  => p (N t)
  -> p (N t)
  -> Maybe (Angle (N (Diff p (N t))))
  -> Maybe (Angle (N (Diff p (N t))))
  -> t
edgeSymbol formPoint toPoint anglesFrom anglesTo = fromSegments [bezier3 offsetToControl1 offsetToControl2 offsetToEnd] where
  angleFrom = fromMaybe (3/4 @@ turn) anglesFrom  -- } edges defaults to go down for unnamed nodes
  angleTo = fromMaybe (1/4 @@ turn) anglesTo  -- }
  offsetToEnd = toPoint .-. formPoint
  offsetToControl1 = rotate angleFrom (scale edgeControlVectorLen unitX)
  offsetToControl2 = rotate angleTo (scale edgeControlVectorLen unitX) ^+^ offsetToEnd