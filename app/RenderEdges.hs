{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, PartialTypeSignatures, ScopedTypeVariables, PatternSynonyms #-}

module RenderEdges(
  makeEdges
  , edgeGraphVizAttrs
  )where

import qualified Diagrams.Prelude as Dia
import           Diagrams.Prelude(
  Angle
  , (.>)
  , arrowBetween'
  )
import qualified Data.GraphViz.Attributes.Complete as GVA
import qualified Data.Graph.Inductive as ING
import GHC.Stack(HasCallStack)
import qualified Data.Text.Lazy as T
import Control.Arrow(first)

--import qualified Data.GraphViz.Types
--import Data.GraphViz.Commands

import EdgeToDiagram( 
  getArrowShadowOpts
  , getArrowBaseOpts
  )
import EdgeAngles(getPortAngle)

import Types(NodeName(..), 
  Connection  
  , EmbedInfo(..)
  , Edge(..)
  , NameAndPort(..)
  , SpecialDiagram
  , SpecialBackend
  , NumericType
  , NamedIcon
  , IconInfo
  , Named(..)
  , EdgeOption(..)
  , PointType
  , ColorStyle
  , Port(..)
  )

import Util(fromMaybeError)

import NodeRecordLabels(showNamedPortRrecord)

import Icons(findIconFromName)
  
-- MinLen - Minimum edge length (rank difference between head and tail).
-- https://www.graphviz.org/doc/info/attrs.html#a:constraint
dontConstrainAttrs :: [GVA.Attribute]
dontConstrainAttrs = [GVA.Constraint False]

importantAttrs :: [GVA.Attribute]
importantAttrs = [GVA.Weight $ GVA.Int 2]

edgeGraphVizAttrs :: (a, Int, EmbedInfo Edge) -> [GVA.Attribute]
edgeGraphVizAttrs (_, _, EmbedInfo _ (Edge option connection)) = attrs where
  attrs = constrainAttrs option ++ placementAttrs connection

constrainAttrs :: EdgeOption -> [GVA.Attribute]
constrainAttrs (DoNotDrawButConstraint len) = [GVA.MinLen len]
constrainAttrs DrawAndNotConstraint {} = dontConstrainAttrs
constrainAttrs DrawAsImportant = importantAttrs
constrainAttrs _  = []

placementAttrs :: Connection -> [GVA.Attribute]
placementAttrs (namedPortFrom, namedPortTo) = [
    GVA.TailPort $ gvaEdgePort (namedPortFrom, Nothing)
  , GVA.HeadPort $ gvaEdgePort (namedPortTo,   Nothing)
  ]

gvaEdgePort :: (NameAndPort, Maybe GVA.CompassPoint) -> GVA.PortPos
gvaEdgePort pair = uncurry GVA.LabelledPort $ first (GVA.PN . T.pack . showNamedPortRrecord) pair

-- | makeEdges draws the edges underneath the nodes.
makeEdges :: (HasCallStack, SpecialBackend b) =>
  ColorStyle
  -> IconInfo
  -> ING.Gr NamedIcon (EmbedInfo Edge)
  -> SpecialDiagram b
  -> SpecialDiagram b
makeEdges colorStyle iconInfo graph origDia
  = mconcat $ map (connectMaybePorts colorStyle iconInfo origDia) labledEdges where
    labledEdges = ING.labEdges graph


-- | Given an Edge, return a transformation on Diagrams that will draw a line.
connectMaybePorts :: (SpecialBackend b)
  => 
  ColorStyle
  -> IconInfo 
  -> SpecialDiagram b
  -> ING.LEdge (EmbedInfo Edge)
  -> SpecialDiagram b
connectMaybePorts
  colorStyle
  iconInfo
  origDia
  labeledEdge@(
    _node0,
    _node1, 
    EmbedInfo _ (Edge _ namedPorts))
  = let 
      mPointFromAndPointTo  = getPoints origDia namedPorts
    in case mPointFromAndPointTo of
      (Nothing, _) -> mempty
      (_, Nothing) -> mempty
      (Just pointFrom, Just pointTo) -> makeArrowDiagram colorStyle iconInfo (pointFrom,pointTo) labeledEdge

getPoints :: SpecialBackend b => SpecialDiagram b-> (NameAndPort, NameAndPort) -> (Maybe PointType, Maybe PointType)
getPoints origDia (fromNamePort, toNamePort) = (pointFrom, pointTo) where
  pointFrom  = getPositionOfNamed origDia fromNamePort
  pointTo = getPositionOfNamed origDia toNamePort

getPositionOfNamed :: SpecialBackend b => SpecialDiagram b-> NameAndPort ->  Maybe PointType
getPositionOfNamed origDia (Named name port) = case Dia.lookupName (name .> port) origDia of
  --Nothing -> Dia.r2 (0, 0)--error "Name does not exist!"
  Nothing -> Nothing -- error $ "Name does not exist! name=" <> show n -- <> "\neInfo=" <> show eInfo
  Just subDia -> Just $ Dia.location subDia

makeArrowDiagram :: (SpecialBackend b)=> 
  ColorStyle 
  -> IconInfo 
  -> (PointType, PointType) 
  -> (Int, Int, EmbedInfo Edge) 
  -> SpecialDiagram b
makeArrowDiagram colorStyle iconInfo pointFromAndPointTo labeledEdge
  = Dia.atop arrowShaft arrowShadow where 
    (arrowBaseOpts, arrowShadowOpts) = getArrowsOpts colorStyle iconInfo labeledEdge pointFromAndPointTo
    arrowShaft = drawArrowFunc arrowBaseOpts pointFromAndPointTo
    arrowShadow = drawArrowFunc arrowShadowOpts pointFromAndPointTo
-- In order to give arrows a "shadow" effect, draw a thicker semi-transparent
-- line shaft the same color as the background underneath the normal line
-- shaft.

drawArrowFunc :: SpecialBackend b =>  Dia.ArrowOpts NumericType -> (PointType, PointType) -> SpecialDiagram b
drawArrowFunc arrowOpts (pointFrom, pointTo) = arrowBetween' arrowOpts pointFrom pointTo


getArrowsOpts :: 
  ColorStyle 
  -> IconInfo 
  -> (Int, Int, EmbedInfo Edge) 
  -> (Dia.Point Dia.V2 NumericType, Dia.Point Dia.V2 NumericType) 
  -> (Dia.ArrowOpts NumericType, Dia.ArrowOpts NumericType)
getArrowsOpts
  colorStyle
  iconInfo
  (node0, node1, EmbedInfo _ e@(Edge  _la namePorts@(fromNamePort, toNamePort))
  )
  points
  = (arrowBaseOpts, arrowShadowOpts) where
    namedIconFrom = findIconFromName iconInfo (NodeName node0)
    namedIconTo = findIconFromName iconInfo (NodeName node1)

    angleFrom = getPortAngle iconInfo (NodeName node0) fromNamePort
    angleTo = getPortAngle iconInfo (NodeName node1) toNamePort

    arrowBaseOpts{-'-} = getArrowBaseOpts namePorts points  (angleFrom, angleTo) (namedIconFrom, namedIconTo) colorStyle
    -- arrowBaseOpts = viewArrowBaseOpts e arrowBaseOpts'
    arrowShadowOpts = getArrowShadowOpts namePorts points  (angleFrom, angleTo) colorStyle

-- viewArrowBaseOpts e arrowBaseOpts' = Dia.shaftStyle Dia.%~ (Dia.opacity 1 $ Dia.lc (shaftColor e))  $ arrowBaseOpts' where
--   shaftColor (Edge DrawAndNotConstraint _ ) = Dia.red
--   shaftColor (Edge (DoNotDrawButConstraint {}) _) = Dia.blue
--   shaftColor _ = Dia.white
-- End makeEdges --