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
  , NumericType
  , NamedIcon
  , IconInfo
  , Named(..)
  , EdgeOption(..)
  , PointType
  , ColorStyle
  , ColorStyle'(..) 
  )

import Util(fromMaybeError)
  
  -- MinLen - Minimum edge length (rank difference between head and tail).
dontConstrainAttrs :: [GVA.Attribute]
dontConstrainAttrs = [GVA.Constraint False, GVA.MinLen 0]

edgeGraphVizAttrs :: (a, Int, EmbedInfo Edge) -> [GVA.Attribute]
edgeGraphVizAttrs (_, _, EmbedInfo _ (Edge (DoNotDrawButConstraint len) _)) 
  = [GVA.MinLen len]
edgeGraphVizAttrs (_, _, EmbedInfo _ (Edge DrawAndNotConstraint _)) = dontConstrainAttrs
edgeGraphVizAttrs _  = []

-- | makeEdges draws the edges underneath the nodes.
makeEdges :: (HasCallStack, SpecialBackend b, ING.Graph gr) =>
  ColorStyle
  -> IconInfo
  -> gr NamedIcon (EmbedInfo Edge)
  -> SpecialDiagram b
  -> SpecialDiagram b
makeEdges colorStyle iconInfo graph origDia
  = mconcat $ map (connectMaybePorts colorStyle iconInfo graph origDia) labledEdges where
    labledEdges = ING.labEdges graph


-- | Given an Edge, return a transformation on Diagrams that will draw a line.
connectMaybePorts :: (ING.Graph gr,SpecialBackend b)
  => 
  ColorStyle
  -> IconInfo 
  -> gr NamedIcon (EmbedInfo Edge)
  -> SpecialDiagram b
  -> ING.LEdge (EmbedInfo Edge)
  -> SpecialDiagram b
connectMaybePorts
  colorStyle
  iconInfo
  graph
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
      (Just pointFrom, Just pointTo) -> makeArrowDiagram colorStyle iconInfo (pointFrom,pointTo) graph labeledEdge

getPoints :: SpecialBackend b => SpecialDiagram b-> (NameAndPort, NameAndPort) -> (Maybe PointType, Maybe PointType)
getPoints origDia (fromNamePort, toNamePort) = (pointFrom, pointTo) where
  pointFrom  = getPositionOfNamed origDia fromNamePort
  pointTo = getPositionOfNamed origDia toNamePort

getPositionOfNamed :: SpecialBackend b => SpecialDiagram b-> NameAndPort ->  Maybe PointType
getPositionOfNamed origDia (NameAndPort name port) = case Dia.lookupName (name .> port) origDia of
  --Nothing -> Dia.r2 (0, 0)--error "Name does not exist!"
  Nothing -> Nothing -- error $ "Name does not exist! name=" <> show n -- <> "\neInfo=" <> show eInfo
  Just subDia -> Just $ Dia.location subDia

makeArrowDiagram :: (SpecialBackend b , ING.Graph gr)=> 
  ColorStyle 
  -> IconInfo 
  -> (PointType, PointType) 
  -> gr NamedIcon (EmbedInfo Edge) 
  -> (Int, Int, EmbedInfo Edge) 
  -> SpecialDiagram b
makeArrowDiagram colorStyle iconInfo pointFromAndPointTo graph labeledEdge
  = Dia.atop arrowShaft arrowShadow where 
    (arrowBaseOpts, arrowShadowOpts) = getArrowsOpts colorStyle iconInfo graph labeledEdge pointFromAndPointTo
    arrowShaft = drawArrowFunc arrowBaseOpts pointFromAndPointTo
    arrowShadow = drawArrowFunc arrowShadowOpts pointFromAndPointTo
-- In order to give arrows a "shadow" effect, draw a thicker semi-transparent
-- line shaft the same color as the background underneath the normal line
-- shaft.

drawArrowFunc :: SpecialBackend b =>  Dia.ArrowOpts NumericType -> (PointType, PointType) -> SpecialDiagram b
drawArrowFunc arrowOpts (pointFrom, pointTo) = arrowBetween' arrowOpts pointFrom pointTo


getArrowsOpts
  colorStyle
  iconInfo
  graph
  (node0
  , node1
  , EmbedInfo _ e@(Edge  _la namePorts@(fromNamePort, toNamePort))
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
    -- arrowBaseOpts = viewArrowBaseOpts e arrowBaseOpts'
    arrowShadowOpts = getArrowShadowOpts namePorts points  (angleFrom, angleTo) (namedIconFrom, namedIconTo) colorStyle

-- viewArrowBaseOpts e arrowBaseOpts' = Dia.shaftStyle Dia.%~ (Dia.opacity 1 $ Dia.lc (shaftColor e))  $ arrowBaseOpts' where
--   shaftColor (Edge DrawAndNotConstraint _ ) = Dia.red
--   shaftColor (Edge (DoNotDrawButConstraint {}) _) = Dia.blue
--   shaftColor _ = Dia.white

findPortAngles :: IconInfo -> NamedIcon -> NameAndPort -> Maybe (Angle NumericType)
findPortAngles iconInfo (Named nodeName nodeIcon) (NameAndPort diaName port)
  = foundAngles where
    mName = if nodeName == diaName then Nothing else Just diaName
    foundAngles = Just $ getPortAngle iconInfo nodeIcon port mName
-- End makeEdges --