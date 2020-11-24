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
import qualified Data.IntMap as IMap

import qualified Data.Graph.Inductive as ING
import GHC.Stack(HasCallStack)

--import qualified Data.GraphViz.Types
--import Data.GraphViz.Commands

import EdgeToDiagram( 
  getArrowShadowOpts
  , getArrowBaseOpts
  )
import EdgeAngles(getPortAngle)

import Types( 
  EmbedInfo(..)
  , Edge(..)
  , NameAndPort(..)
  , SpecialDiagram
  , SpecialBackend
  , SpecialNum
  , NamedIcon
  , Icon(..)
  , IconInfo
  , Named(..)
  , EdgeOption(..)
  , DiagramIcon(..)
  )

import Util(fromMaybeError)

import PortConstants(pattern InputPortConst)
import DrawingColors (ColorStyle)
  
  -- MinLen - Minimum edge length (rank difference between head and tail).
dontConstrainAttrs :: [GVA.Attribute]
dontConstrainAttrs = [GVA.Constraint False, GVA.MinLen 0]

edgeGraphVizAttrs :: IconInfo -> (a, Int, EmbedInfo Edge) -> [GVA.Attribute]
edgeGraphVizAttrs iconInfo (_, iconNameTo, EmbedInfo _ (Edge DoNotDrawButConstraint _)) 
  = case iconInfo IMap.! iconNameTo of
    (Icon (FunctionDefIcon _ (_,level) _) _) -> [ GVA.MinLen level ]
    _ -> []
  
edgeGraphVizAttrs _ (_, _, EmbedInfo _ (Edge DrawAndNotConstraint _)) = dontConstrainAttrs
edgeGraphVizAttrs _ (_, _, EmbedInfo _ (Edge _ (_,NameAndPort _ InputPortConst))) = [] 
edgeGraphVizAttrs iconInfo (_nFrom, iconNameTo, _) = case iconInfo IMap.! iconNameTo of
  (Icon FunctionDefIcon {} _) -> dontConstrainAttrs
  _ -> []

-- edgeGraphVizAttrs _ = []

-- | makeEdges draws the edges underneath the nodes.
makeEdges :: (HasCallStack, SpecialBackend b n, ING.Graph gr) =>
  ColorStyle Double
  -> IconInfo
  -> gr NamedIcon (EmbedInfo Edge)
  -> SpecialDiagram b n
  -> SpecialDiagram b n
makeEdges colorStyle iconInfo graph
  = mconcat $ map (connectMaybePorts colorStyle iconInfo graph) labledEdges
    where labledEdges = ING.labEdges graph

-- | Given an Edge, return a transformation on Diagrams that will draw a line.
connectMaybePorts ::  (ING.Graph gr,SpecialBackend b n)
  => 
  ColorStyle Double
  -> IconInfo 
  -> gr NamedIcon (EmbedInfo Edge)
  -> ING.LEdge (EmbedInfo Edge)
  -> SpecialDiagram b n
  -> SpecialDiagram b n
connectMaybePorts
  colorStyle
  iconInfo
  graph
  labeledEdge@(
    _node0,
    _node1, 
    EmbedInfo _ (Edge _ namedPorts))
  origDia
  = let 
      mPointFromAndPointTo  = getPoints origDia namedPorts
    in case mPointFromAndPointTo of
      (Nothing, _) -> mempty
      (_, Nothing) -> mempty
      (Just pointFrom, Just pointTo) -> makeArrowDiagram colorStyle iconInfo (pointFrom,pointTo) graph labeledEdge

getPoints :: SpecialBackend b n => SpecialDiagram b n-> (NameAndPort, NameAndPort) -> (Maybe (Dia.Point Dia.V2 n), Maybe (Dia.Point Dia.V2 n))
getPoints origDia (fromNamePort, toNamePort) = (pointFrom, pointTo) where
  pointFrom  = getPositionOfNamed origDia fromNamePort
  pointTo = getPositionOfNamed origDia toNamePort

getPositionOfNamed :: SpecialBackend b n => SpecialDiagram b n-> NameAndPort ->  Maybe (Dia.Point Dia.V2 n)
getPositionOfNamed origDia (NameAndPort name port) = case Dia.lookupName (name .> port) origDia of
  --Nothing -> Dia.r2 (0, 0)--error "Name does not exist!"
  Nothing -> Nothing -- error $ "Name does not exist! name=" <> show n -- <> "\neInfo=" <> show eInfo
  Just subDia -> Just $ Dia.location subDia

makeArrowDiagram :: (SpecialBackend b n , ING.Graph gr)=> 
  ColorStyle Double 
  -> IconInfo 
  -> (Dia.Point Dia.V2 n, Dia.Point Dia.V2 n) 
  -> gr NamedIcon (EmbedInfo Edge) 
  -> (Int, Int, EmbedInfo Edge) 
  -> SpecialDiagram b n
makeArrowDiagram colorStyle iconInfo pointFromAndPointTo graph labeledEdge
  = Dia.atop arrowShaft arrowShadow where 
    (arrowBaseOpts, arrowShadowOpts) = getArrowsOpts colorStyle iconInfo graph labeledEdge pointFromAndPointTo
    arrowShaft = drawArrowFunc arrowBaseOpts pointFromAndPointTo
    arrowShadow = drawArrowFunc arrowShadowOpts pointFromAndPointTo
-- In order to give arrows a "shadow" effect, draw a thicker semi-transparent
-- line shaft the same color as the background underneath the normal line
-- shaft.

drawArrowFunc :: SpecialBackend b n =>  Dia.ArrowOpts n -> (Dia.Point Dia.V2 n, Dia.Point Dia.V2 n)  -> SpecialDiagram b n
drawArrowFunc arrowOpts (pointFrom, pointTo) = arrowBetween' arrowOpts pointFrom pointTo


getArrowsOpts
  colorStyle
  iconInfo
  graph
  (node0
  , node1
  , EmbedInfo _ (Edge  _ namePorts@(fromNamePort, toNamePort))
  )
  points
  = (arrowBaseOpts, arrowShadowOpts) where
    namedIconFrom = fromMaybeError ("makeEdge: nodeFrom is not in graph: " ++ show node0)
                $ ING.lab graph node0
    namedIconTo = fromMaybeError ("makeEdge: nodeTo is not in graph: " ++ show node1)
                $ ING.lab graph node1

    angleFrom = findPortAngles iconInfo namedIconFrom fromNamePort
    angleTo = findPortAngles iconInfo namedIconTo toNamePort

    arrowBaseOpts{-'-} = getArrowBaseOpts namePorts points  (angleFrom, angleTo) (namedIconFrom, namedIconTo) colorStyle
    -- arrowBaseOpts = Dia.shaftStyle Dia.%~ ( Dia.lc (shaftColor e))  $ arrowBaseOpts'
    arrowShadowOpts = getArrowShadowOpts namePorts points  (angleFrom, angleTo) (namedIconFrom, namedIconTo) colorStyle

-- shaftColor (Edge DrawAndNotConstraint _) = Dia.red
-- shaftColor (Edge DoNotDrawButConstraint _) = Dia.blue
-- shaftColor _ = Dia.white

findPortAngles :: SpecialNum n
  => IconInfo -> NamedIcon -> NameAndPort -> Maybe (Angle n)
findPortAngles iconInfo (Named nodeName nodeIcon) (NameAndPort diaName port)
  = foundAngles where
    mName = if nodeName == diaName then Nothing else Just diaName
    foundAngles = Just $ getPortAngle iconInfo nodeIcon port mName
-- End makeEdges --