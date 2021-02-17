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
  , ArrowPoints
  )
import EdgeAngles(getPortAngle)

import Types(NodeName(..), 
  Connection  
  , EmbedInfo(..)
  , Edge(..)
  , NameAndPort(..)
  , Drawing
  , DrawingBackend
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
constrainAttrs DrawThrough {}  = dontConstrainAttrs
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
makeEdges :: (HasCallStack, DrawingBackend b) =>
  ColorStyle
  -> IconInfo
  -> ING.Gr NamedIcon (EmbedInfo Edge)
  -> Drawing b
  -> Drawing b
makeEdges colorStyle iconInfo graph origDia
  = mconcat $ map (connectMaybePorts colorStyle iconInfo origDia) labledEdges where
    labledEdges = ING.labEdges graph


-- | Given an Edge, return a transformation on Diagrams that will draw a line.
connectMaybePorts :: (DrawingBackend b)
  => 
  ColorStyle
  -> IconInfo 
  -> Drawing b
  -> ING.LEdge (EmbedInfo Edge)
  -> Drawing b
connectMaybePorts
  colorStyle
  iconInfo
  origDia
  labeledEdge@(
    _node0,
    _node1, 
    EmbedInfo _ (Edge option namedPorts))
  = let 
      mPointFromAndPointTo  = getPoints origDia namedPorts
      mMiddlePoints = getMiddle origDia option
    in case mPointFromAndPointTo of
      Nothing -> mempty
      (Just points) -> makeArrowDiagram colorStyle iconInfo (points, mMiddlePoints) labeledEdge

getPoints :: DrawingBackend b => Drawing b-> (NameAndPort, NameAndPort) -> Maybe(PointType, PointType)
getPoints origDia (fromNamePort, toNamePort) =  do
  pointFrom  <- getPositionOfNamed origDia fromNamePort
  pointTo <- getPositionOfNamed origDia toNamePort
  return (pointFrom, pointTo)

getMiddle :: DrawingBackend b => Drawing b -> EdgeOption -> Maybe (PointType, PointType)
getMiddle origDia (DrawThrough middleNamePorts) = getPoints origDia middleNamePorts
getMiddle _ _ = Nothing

getPositionOfNamed :: DrawingBackend b => Drawing b-> NameAndPort ->  Maybe PointType
getPositionOfNamed origDia (Named name port) = case Dia.lookupName (name .> port) origDia of
  --Nothing -> Dia.r2 (0, 0)--error "Name does not exist!"
  Nothing -> Nothing -- error $ "Name does not exist! name=" <> show n -- <> "\neInfo=" <> show eInfo
  Just subDia -> Just $ Dia.location subDia

makeArrowDiagram :: (DrawingBackend b)=> 
  ColorStyle 
  -> IconInfo 
  -> ArrowPoints 
  -> (Int, Int, EmbedInfo Edge) 
  -> Drawing b
makeArrowDiagram colorStyle iconInfo points@(fromAndTo,_) labeledEdge = arrowDia where
  arrowDia =  Dia.atop arrowShaft arrowShadow 
  (arrowBaseOpts, arrowShadowOpts) = getArrowsOpts colorStyle iconInfo labeledEdge points
  arrowShaft = drawArrowFunc arrowBaseOpts fromAndTo
  arrowShadow = drawArrowFunc arrowShadowOpts fromAndTo
-- In order to give arrows a "shadow" effect, draw a thicker semi-transparent
-- line shaft the same color as the background underneath the normal line
-- shaft.

drawArrowFunc :: DrawingBackend b =>  Dia.ArrowOpts NumericType -> (PointType, PointType) -> Drawing b
drawArrowFunc arrowOpts (pointFrom, pointTo) = arrowBetween' arrowOpts pointFrom pointTo


getArrowsOpts :: 
  ColorStyle 
  -> IconInfo 
  -> (Int, Int, EmbedInfo Edge) 
  -> ArrowPoints
  -> (Dia.ArrowOpts NumericType, Dia.ArrowOpts NumericType)
getArrowsOpts colorStyle iconInfo (node0, node1, EmbedInfo _ edge) points = (arrowBaseOpts, arrowShadowOpts) where
  Edge _ namePorts@(fromNamePort, toNamePort) = edge

  namedIconFrom = findIconFromName iconInfo (NodeName node0)
  namedIconTo = findIconFromName iconInfo (NodeName node1)

  angleFrom = getPortAngle iconInfo (NodeName node0) fromNamePort
  angleTo = getPortAngle iconInfo (NodeName node1) toNamePort

  arrowBaseOpts{-'-} = getArrowBaseOpts namePorts points (angleFrom, angleTo) (namedIconFrom, namedIconTo) colorStyle
  -- arrowBaseOpts = viewArrowBaseOpts e arrowBaseOpts'
  arrowShadowOpts = getArrowShadowOpts namePorts points (angleFrom, angleTo) colorStyle

-- viewArrowBaseOpts e arrowBaseOpts' = Dia.shaftStyle Dia.%~ (Dia.opacity 1 $ Dia.lc (shaftColor e))  $ arrowBaseOpts' where
--   shaftColor (Edge DrawAndNotConstraint _ ) = Dia.red
--   shaftColor (Edge (DoNotDrawButConstraint {}) _) = Dia.blue
--   shaftColor _ = Dia.white
-- End makeEdges --