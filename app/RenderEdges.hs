{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, PartialTypeSignatures, ScopedTypeVariables, PatternSynonyms #-}

module RenderEdges(
  makeEdges
  , edgeGraphVizAttrs
  )where

import qualified Diagrams.Prelude as Dia
import           Diagrams.Prelude               ( toName
                                                , Angle
                                                , P2
                                                , height
                                                , width
                                                , (*^)
                                                , centerXY
                                                , place
                                                , applyAll
                                                , (.>)
                                                , connectOutside'
                                                , arrowBetween'
                                                , (*^)
                                                )
import Diagrams.TwoD.GraphViz(mkGraph, getGraph, layoutGraph')
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GVA
import qualified Data.IntMap as IMap
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Arrow(first)
import qualified Data.Graph.Inductive as ING
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List(find)
import Data.Maybe(isNothing, mapMaybe)
import GHC.Stack(HasCallStack)

--import qualified Data.GraphViz.Types
--import Data.GraphViz.Commands

import Icons(findMaybeIconFromName)
import IconToSymbolDiagram  ( 
  getArrowShadowOpts
  , getArrowBaseOpts
  )
import EdgeAngles(getPortAngle)

import SyntaxNodeToIcon(nodeToIcon)
import Types(EmbedInfo(..), AnnotatedGraph, Edge(..)
            , Drawing(..), NameAndPort(..)
            , SpecialDiagram, SpecialBackend, SpecialNum, NodeName(..)
            , NamedIcon, Icon(..), NodeInfo(..), IconInfo
            , Named(..)
            , TransformParams(..)
            , EdgeOption(..)
            , DiagramIcon(..)
            , Port(..)
            )

import Util(nodeNameToInt, fromMaybeError, namedToTuple)
import ClusterNodesBy (
  clusterNodesBy
  , ClusterT
  )

import PortConstants(
  pattern InputPortConst
  , pattern ResultPortConst
  )
  
  -- MinLen - Minimum edge length (rank difference between head and tail).
dontConstrainAttrs :: [GVA.Attribute]
dontConstrainAttrs = [ GVA.Constraint False, GVA.MinLen 0]

edgeGraphVizAttrs :: IconInfo -> (a, Int, EmbedInfo Edge) -> [GVA.Attribute]
edgeGraphVizAttrs iconInfo (_, iconNameTo, EmbedInfo _ (Edge DoNotDrawButConstraint _)) 
  = case iconInfo IMap.! iconNameTo of
    (Icon (FunctionDefIcon _ (_,level) _) _) -> [ GVA.MinLen (2 * level + 1) ]
    _ -> []
  
edgeGraphVizAttrs _ (_, _, EmbedInfo _ (Edge DrawAndNotConstraint _)) = dontConstrainAttrs
edgeGraphVizAttrs _ (_, _, EmbedInfo _ (Edge _ (_,NameAndPort _ InputPortConst))) = [] 
edgeGraphVizAttrs iconInfo (_nFrom, iconNameTo, _) = case iconInfo IMap.! iconNameTo of
  (Icon (FunctionDefIcon {}) _) -> dontConstrainAttrs
  _ -> []

-- edgeGraphVizAttrs _ = []

-- | makeEdges draws the edges underneath the nodes.
makeEdges :: (HasCallStack, SpecialBackend b n, ING.Graph gr) =>
  String  -- ^ Debugging information
  -> IconInfo
  -> gr NamedIcon (EmbedInfo Edge)
  -> SpecialDiagram b n
  -> SpecialDiagram b n
makeEdges _debugInfo iconInfo graph
  = mconcat $ map (connectMaybePorts  iconInfo graph) labledEdges
    where labledEdges = ING.labEdges graph

-- | Given an Edge, return a transformation on Diagrams that will draw a line.
connectMaybePorts ::  (ING.Graph gr,SpecialBackend b n)
  => IconInfo 
  -> gr NamedIcon (EmbedInfo Edge)
  -> ING.LEdge (EmbedInfo Edge)
  -> SpecialDiagram b n
  -> SpecialDiagram b n
connectMaybePorts
  iconInfo
  graph
  labeledEdge@((_node0, _node1, 
    (EmbedInfo _embedDir
    (Edge
      _edgeOptions
      (fromNamePort, toNamePort)))))
  origDia
  = let 
      mPointFromAndPointTo  = getPoints origDia fromNamePort toNamePort
    in case mPointFromAndPointTo of
      (Nothing, _) -> mempty
      (_, Nothing) -> mempty
      (Just pointFrom, Just pointTo) -> makeArrowDiagram iconInfo (pointFrom,pointTo) graph labeledEdge

getPoints origDia (NameAndPort name0 port0) (NameAndPort name1 port1) = (pointFrom, pointTo) where
  qPort0 = name0 .> port0
  qPort1 = name1 .> port1
  pointFrom  = getPositionOfNamed origDia qPort0
  pointTo = getPositionOfNamed origDia qPort1

getPositionOfNamed origDia n = case Dia.lookupName n origDia of
  --Nothing -> Dia.r2 (0, 0)--error "Name does not exist!"
  Nothing -> Nothing -- error $ "Name does not exist! name=" <> show n -- <> "\neInfo=" <> show eInfo
  Just subDia -> Just $ Dia.location subDia

makeArrowDiagram iconInfo pointFromAndPointTo graph labeledEdge
  = Dia.atop arrowShaft arrowShadow where 
    (arrowBaseOpts, arrowShadowOpts) = getArrowsOpts iconInfo graph labeledEdge pointFromAndPointTo
    arrowShaft = drawArrowFunc arrowBaseOpts pointFromAndPointTo
    arrowShadow = drawArrowFunc arrowShadowOpts pointFromAndPointTo
-- In order to give arrows a "shadow" effect, draw a thicker semi-transparent
-- line shaft the same color as the background underneath the normal line
-- shaft.

drawArrowFunc arrowOpts (pointFrom, pointTo) = arrowBetween' arrowOpts pointFrom pointTo

getArrowsOpts
  iconInfo
  graph
  (node0, node1, 
    (EmbedInfo _embedDir --edge@(EmbedInfo _ (Edge _ (_namePort0, _namePort1)))
    e@(Edge
      _
      namePorts@(fromNamePort, toNamePort))))
  (pointFrom, pointTo)
  = (arrowBaseOpts, arrowShadowOpts) where
    namedIconFrom = fromMaybeError ("makeEdge: nodeFrom is not in graph: " ++ show node0)
                $ ING.lab graph node0
    namedIconTo = fromMaybeError ("makeEdge: nodeTo is not in graph: " ++ show node1)
                $ ING.lab graph node1

    angleFrom = findPortAngles iconInfo namedIconFrom fromNamePort
    angleTo = findPortAngles iconInfo namedIconTo toNamePort

    arrowBaseOpts{-'-} = getArrowBaseOpts namePorts (pointFrom, pointTo)  (angleFrom, angleTo) (namedIconFrom, namedIconTo)
    -- arrowBaseOpts = Dia.shaftStyle Dia.%~ ( Dia.lc (shaftColor e))  $ arrowBaseOpts'
    arrowShadowOpts = getArrowShadowOpts namePorts (pointFrom, pointTo)  (angleFrom, angleTo) (namedIconFrom, namedIconTo)

shaftColor (Edge DrawAndNotConstraint _) = Dia.red
shaftColor (Edge DoNotDrawButConstraint _) = Dia.blue
shaftColor _ = Dia.white

findPortAngles :: SpecialNum n
  => IconInfo -> NamedIcon -> NameAndPort -> Maybe (Angle n)
findPortAngles iconInfo (Named nodeName nodeIcon) (NameAndPort diaName port)
  = foundAngles where
    mName = if nodeName == diaName then Nothing else Just diaName
    foundAngles = Just $ getPortAngle iconInfo nodeIcon port mName
-- End makeEdges --