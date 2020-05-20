{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, PartialTypeSignatures, ScopedTypeVariables #-}

module RenderEdges(
  addEdges
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
                                                , connect'
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
import IconToSymbolDiagram  ( iconToDiagram
                , lambdaRegionToDiagram
                , getArrowShadowOpts
                , getArrowBaseOpts
                )
import EdgeAngles(getPortAngle)

import SyntaxNodeToIcon(nodeToIcon)
import Types(EmbedInfo(..), AnnotatedGraph, Edge(..)
            , Drawing(..), NameAndPort(..)
            , SpecialQDiagram, SpecialBackend, SpecialNum, NodeName(..)
            , NamedIcon, Icon(..), NodeInfo(..), IconInfo
            , Named(..)
            , TransformParams(..)
            , EdgeOption(..)
            )

import Util(nodeNameToInt, fromMaybeError, namedToTuple)
import ClusterNodesBy (
  clusterNodesBy
  , ClusterT
  ) 
  

edgeGraphVizAttrs :: (a, b, EmbedInfo Edge) -> [GVA.Attribute]
edgeGraphVizAttrs (_nFrom, _nTo, (EmbedInfo _ (Edge DrawAndNotConstraint _))) = [GVA.Constraint False]
-- edgeGraphVizAttrs (_nFrom, _nTo, (EmbedInfo _ (Edge DoNotDrawButConstraint _))) = [GVA.Weight $ GVA.Dbl 0.1]
edgeGraphVizAttrs _ = []

-- | addEdges draws the edges underneath the nodes.
addEdges :: (HasCallStack, SpecialBackend b n, ING.Graph gr) =>
  String  -- ^ Debugging information
  -> IconInfo
  -> gr NamedIcon (EmbedInfo Edge)
  -> SpecialQDiagram b n
  -> SpecialQDiagram b n
addEdges _debugInfo iconInfo graph = applyAll connections
  where
            connections = makeEdge  iconInfo graph <$> ING.labEdges graph

makeEdge :: (ING.Graph gr,HasCallStack, SpecialBackend b n) 
  => IconInfo
  -> gr NamedIcon (EmbedInfo Edge)
  -> ING.LEdge (EmbedInfo Edge)
  -> SpecialQDiagram b n
  -> SpecialQDiagram b n
makeEdge _ _ (_, _,(EmbedInfo _ (Edge DoNotDrawButConstraint _))) origDia
  = origDia
makeEdge iconInfo graph lEdge origDia
  = connectMaybePorts iconInfo graph lEdge origDia

-- | Given an Edge, return a transformation on Diagrams that will draw a line.
connectMaybePorts ::  (ING.Graph gr,SpecialBackend b n)
  => IconInfo 
  -> gr NamedIcon (EmbedInfo Edge)
  -> ING.LEdge (EmbedInfo Edge)
  -> SpecialQDiagram b n
  -> SpecialQDiagram b n
connectMaybePorts
  iconInfo
  graph
  labeledEdge@((_node0, _node1, 
    (EmbedInfo _embedDir
    (Edge
      _edgeOptions
      (fromNamePort, toNamePort)))))
  origDia
  = newDia where 
    (connectFunc, qPort0, qPort1) = getConnectFuncAndPorts fromNamePort toNamePort

    pointFrom  = getPositionOfNamed origDia qPort0
    pointTo = getPositionOfNamed origDia qPort1

    newDia = case (pointFrom, pointTo) of
      (Nothing,Nothing) -> origDia
      (Nothing,_) -> origDia
      (_,Nothing) -> origDia
      (_, _) -> ((connectFunc arrowBaseOpts qPort0 qPort1) . (connectFunc arrowShadowOpts qPort0 qPort1)) origDia where
        (arrowBaseOpts,arrowShadowOpts) = getArrowsOpts iconInfo    graph   labeledEdge     pointFrom    pointTo

getConnectFuncAndPorts  (NameAndPort name0 mPort1) (NameAndPort name1 mPort2) = helper (mPort1, mPort2) where
      helper (Just port0, Just port1) = (connect', name0 .> port0, name1 .> port1)
      helper (Nothing, Just port1) = (connectOutside', toName name0, name1 .> port1)
      helper (Just port0, Nothing) = (connectOutside', name0 .> port0, toName name1)
      helper (_, _) = (connectOutside', toName name0, toName name1)

getPositionOfNamed origDia n = case Dia.lookupName n origDia of
  --Nothing -> Dia.r2 (0, 0)--error "Name does not exist!"
  Nothing -> Nothing-- error $ "Name does not exist! name=" <> show n <> "\neInfo=" <> show eInfo
  Just subDia -> Just $ Dia.location subDia

-- In order to give arrows a "shadow" effect, draw a thicker semi-transparent
-- line shaft the same color as the background underneath the normal line
-- shaft.

getArrowsOpts
  iconInfo
  graph
  (node0, node1, 
    (EmbedInfo _embedDir --edge@(EmbedInfo _ (Edge _ (_namePort0, _namePort1)))
    (Edge
      _
      (fromNamePort, toNamePort))))
  pointFrom
  pointTo
  = (arrowBaseOpts, arrowShadowOpts) where
    node0NameAndPort = fromMaybeError
                ("makeEdge: node0 is not in graph. node0: " ++ show node0)
                $ ING.lab graph node0
    node1NameAndPort = fromMaybeError
                ("makeEdge: node1 is not in graph. node1: " ++ show node1)
                $ ING.lab graph node1

    angleFrom = findPortAngles iconInfo node0NameAndPort fromNamePort
    angleTo = findPortAngles iconInfo node1NameAndPort toNamePort

    arrowShadowOpts = getArrowShadowOpts (pointFrom, pointTo)  (angleFrom, angleTo) 
    arrowBaseOpts = getArrowBaseOpts fromNamePort (pointFrom, pointTo)  (angleFrom, angleTo) 



findPortAngles :: SpecialNum n
  => IconInfo -> NamedIcon -> NameAndPort -> Maybe (Angle n)
findPortAngles iconInfo (Named nodeName nodeIcon) (NameAndPort diaName mPort)
  = case mPort of
      Nothing -> Nothing
      Just port -> foundAngles where
        mName = if nodeName == diaName then Nothing else Just diaName
        foundAngles = Just $ getPortAngle iconInfo nodeIcon port mName
-- End addEdges --