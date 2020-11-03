{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module EdgeToDiagram(
  getArrowShadowOpts
  , getArrowBaseOpts
  ) where

import Diagrams.Prelude hiding ((&), (#), Name)
import Data.Maybe(fromMaybe, isNothing)
import Data.Typeable(Typeable)

import DrawingColors(colorScheme, ColorStyle(..))

import PortConstants(
  pattern InputPortConst
  , pattern ResultPortConst
  , argPortsConst
  , resultPortsConst
  , isArgPort
  , mixedPorts
  , pattern PatternUnpackingPort
  , listFromPort
  , listThenPort
  , listToPort
  , listCompQualPorts
  )

import Types(
  Icon(..)
  , DiagramIcon(..)
  , SpecialDiagram
  , SpecialBackend
  , SpecialNum
  , NodeName(..)
  , Port(..)
  , LikeApplyFlavor(..)
  , NamedIcon
  , Labeled(..)
  , IconInfo
  , Named(..)
  , NameAndPort(..)
  , TransformParams(..)
  , TransformableDia
  , CaseOrMultiIfTag(..)
  , SpecialQDiagram 
  , Edge(..)
  , EdgeOption(..)
  , Connection
  )

import DiagramSymbols(
  arrowLineWidth
  , arrowShadowWidth
  , defaultShadowOpacity
  , symbolSize
  )

getArrowShadowOpts :: (RealFloat n, Typeable n)
  => (NameAndPort,NameAndPort)
  -> (Point V2 n, Point V2 n)
  -> (Maybe (Angle n), Maybe (Angle n))
  -> (NamedIcon,NamedIcon)
  -> ArrowOpts n
getArrowShadowOpts 
  (_, namedPortTo)
  points maybeAngles iconPair
  = shaftStyle %~ (lwG arrowShadowWidth .
                lcA $ withOpacity shaftColor defaultShadowOpacity)
  $ headStyle %~ fc shaftColor
  $ getArrowOpts points maybeAngles iconPair namedPortTo where
    shaftColor = backgroundC colorScheme

getArrowBaseOpts :: (RealFloat n, Typeable n)
  => (NameAndPort,NameAndPort)
  -> (Point V2 n, Point V2 n)
  -> (Maybe (Angle n), Maybe (Angle n))
  -> (NamedIcon, NamedIcon)
  -> ArrowOpts n
getArrowBaseOpts 
  namesAndPorts@(_, namedPortTo)
  points maybeAngles 
  iconPair
  = shaftStyle %~ (lwG arrowLineWidth {-- )-- -} . lc shaftColor) 
  $ headStyle %~ fc shaftColor
  $ getArrowOpts points maybeAngles iconPair namedPortTo where
    shaftColor = getShaftColor namesAndPorts iconPair

getShaftColor :: (NameAndPort,NameAndPort) -> (NamedIcon, NamedIcon) -> Colour Double
getShaftColor = getShaftColor' edgeColors where
  edgeColors = edgeListC colorScheme

getShaftColor' :: [Colour Double]
  -> (NameAndPort,NameAndPort)-> (NamedIcon, NamedIcon) -> Colour Double
getShaftColor' _ (NameAndPort _ ResultPortConst,_) (Named _ (Icon FunctionDefIcon {} _), _) = lambdaC colorScheme
getShaftColor' _ (_, NameAndPort _ InputPortConst) (_, Named _ (Icon FunctionDefIcon {} _)) = lambdaC colorScheme
getShaftColor' edgeColors (NameAndPort (NodeName nodeNum) (Port portNum),_) (Named _ (Icon ListCompIcon {} _), _) = hashedShaftColor nodeNum portNum edgeColors
getShaftColor' edgeColors (_, NameAndPort (NodeName nodeNum) (Port portNum)) (_, Named _ (Icon ListCompIcon {} _)) = hashedShaftColor nodeNum (portNum + 1) edgeColors
getShaftColor' edgeColors (NameAndPort (NodeName nodeNum) (Port portNum),_) _ = hashedShaftColor nodeNum portNum edgeColors

hashedShaftColor nodeNum portNum edgeColors = shaftColor where
  namePortHash = mod (portNum + (503 * nodeNum)) (length edgeColors)
  shaftColor = edgeColors !! namePortHash

getArrowOpts :: (RealFloat n, Typeable n)
  => (Point V2 n, Point V2 n)
  -> (Maybe (Angle n), Maybe (Angle n))
  -> (NamedIcon, NamedIcon)
  -> NameAndPort
  -> ArrowOpts n
getArrowOpts (formPoint, toPoint) (anglesFrom,anglesTo) (_,iconTo) namedPortTo
  = arrowOptions where
    arrowOptions =
      -- arrowHead .~ noHead
      arrowHead .~ getArrowHead iconTo namedPortTo
      $ arrowTail .~ noTail
      $ arrowShaft .~ edgeSymbol formPoint toPoint anglesFrom anglesTo
      -- TODO Don't use a magic number for lengths (headLength and tailLength)
      $ lengths .~ global 0.5
      $ with

-- getArrowHead :: Icon -> 
getArrowHead :: RealFloat n => NamedIcon -> NameAndPort -> ArrowHT n
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
  scaleFactor = symbolSize * 8.0
  offsetToEnd = toPoint .-. formPoint
  offsetToControl1 = rotate angleFrom (scale scaleFactor unitX)
  offsetToControl2 = rotate angleTo (scale scaleFactor unitX) ^+^ offsetToEnd