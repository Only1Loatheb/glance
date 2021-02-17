{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, PartialTypeSignatures, ScopedTypeVariables #-}

module Rendering (
  customLayoutParams
  , renderIngSyntaxGraph
) where

import qualified Diagrams.Prelude as Dia

import Diagrams.TwoD.GraphViz(getGraph, layoutGraph')
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GVA
import qualified Data.IntMap as IMap
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Arrow(first)
import qualified Data.Graph.Inductive as ING
import Data.Graph.Inductive.PatriciaTree (Gr) 
import Data.List(find)
import GHC.Stack(HasCallStack)

--import qualified Data.GraphViz.Types
--import Data.GraphViz.Commands

import IconToDiagram( iconToDiagram, lambdaRegionToDiagram)
import NodeRecordLabels(recordLabels) 
import SyntaxNodeToIcon(nodeToIcon)
import           Types (
  EmbedInfo(..)
  , Edge(..)
  , Drawing
  , DrawingBackend
  , NodeName(..)
  , NamedIcon
  , Icon(..)
  , DiagramIcon(..)
  , NodeInfo(..)
  , IconInfo
  , Named(..)
  , DrawingInfo(..)
  , QueryableDrawing
  , AnnotatedFGR
  , ColorStyle
  , InCaseOrInApply(..)
  )

import Util(nodeNameToInt, namedToTuple)
import ClusterNodesBy (
  clusterNodesBy
  , ClusterT
  )
import RenderEdges( makeEdges, edgeGraphVizAttrs)

import NodePlacementMap (
  placeNode
  , getQueryRects
  ) 
import DrawingColors (dummyColorStyle)
import TextBox ( letterHeight)

import ConcentrateEdges(concentrateEdges)
-- If the inferred types for these functions becomes unweildy,
-- try using PartialTypeSignitures.

drawingToGraphvizScaleFactor :: Double
-- For Neato, ScaleOverlaps
--drawingToGraphvizScaleFactor = 0.08
-- has to be set acording to drawings
drawingToGraphvizScaleFactor = 0.13

-- GVA.Width and GVA.Height have a minimum of 0.01
minialGVADimention :: Double
minialGVADimention = 0.01

nodeSeparationX :: Double
nodeSeparationX = 1.5 * letterHeight * drawingToGraphvizScaleFactor
nodeSeparationY :: Double
nodeSeparationY = 2.38 * letterHeight * drawingToGraphvizScaleFactor

drawLambdaRegions :: forall b . DrawingBackend b =>
  ColorStyle 
  -> IconInfo
  -> [(NamedIcon, Drawing b)]
  -> Drawing b
drawLambdaRegions colorStyle iconInfo placedNodes
  = mconcat $ fmap (drawRegion Set.empty . fst) placedNodes
  where
    findDia :: NodeName -> Drawing b
    findDia n1
      = maybe mempty snd
        (find (\(Named n2 _, _) -> n1 == n2) placedNodes)

    -- Also draw the region around the icon the lambda is in.
    -- Consult CollapseGraph to find out where FunctionDefIcon can be nested 
    drawRegion :: Set.Set NodeName -> NamedIcon -> Drawing b
    drawRegion parentNames (Named name (Icon diagramIcon _)) = case diagramIcon of
      (FunctionArgIcon _ (enclosedNames,level) lambdaName)
        -> thisRegionDiagram where
          thisRegionDiagram = lambdaRegionToDiagram colorStyle enclosed name level
          enclosed = findDia <$> lambdaName : Set.toList (parentNames <> enclosedNames)
      _ -> mempty

customLayoutParams :: GV.GraphvizParams ING.Node v e ClusterT v
customLayoutParams = GV.defaultParams{
  GV.globalAttributes = [
      GV.NodeAttrs [GVA.Shape GVA.Record]
    , GV.EdgeAttrs [GVA.MinLen 1]
    , GV.GraphAttrs [
      --GVA.Overlap GVA.KeepOverlaps,
      --GVA.Overlap GVA.ScaleOverlaps,
        GVA.Overlap $ GVA.PrismOverlap (Just 5000)
      , GVA.Splines GVA.Curved
      , GVA.OverlapScaling 8
      --GVA.OverlapScaling 4,
      , GVA.OverlapShrink True
      , GVA.ClusterRank GVA.Local
      , GVA.RankSep [nodeSeparationY]
      , GVA.NodeSep nodeSeparationX
      , GVA.Margin $ GVA.PVal $ GVA.createPoint 0.0 0.0
      ]
    ]
      
    , GV.clusterID =  GV.Num . GV.Int --   ClusterT
  }

getDiagramWidthAndHeight :: forall b. DrawingBackend b => Drawing b -> (Double, Double)
getDiagramWidthAndHeight dummyDiagram = (diaWidth, diaHeight) where
  diaWidth = max (drawingToGraphvizScaleFactor * Dia.width dummyDiagram) minialGVADimention
  diaHeight = max (drawingToGraphvizScaleFactor * Dia.height dummyDiagram) minialGVADimention    

renderIconGraph :: forall b. DrawingBackend b
  => ColorStyle
  -> Gr (NodeInfo NamedIcon) (EmbedInfo Edge)
  -> Gr (NodeInfo NamedIcon) (EmbedInfo Edge)
  -> IO (QueryableDrawing b)
renderIconGraph colorStyle fullGraphWithInfo viewGraph = do
  layoutResult <- layoutGraph' layoutParams GVA.Dot parentGraphNoLoops
  let
    iconAndPositions = (Map.toList . fst . getGraph)  layoutResult
    
    iconAndPlacedNodes :: [(NamedIcon,Drawing b)]
    iconAndPlacedNodes = map (placeNode iconInfo colorStyle drawingToGraphvizScaleFactor) iconAndPositions
    placedNodes = mconcat $ fmap snd iconAndPlacedNodes

    placedRegions = Dia.value mempty $ drawLambdaRegions colorStyle iconInfo iconAndPlacedNodes
    placedEdges = Dia.value mempty $ makeEdges colorStyle iconInfo parentGraph placedNodes
    placedNodesAny = Dia.value mempty placedNodes

    queryRects = mconcat $ getQueryRects iconAndPlacedNodes
    -- boxesDia = mconcat $ map (Dia.lc Dia.blue $ Dia.boundingRect . snd) iconAndBoudingRect
    -- gve = Dia.value mempty $ graphVizEdges layoutResult
  pure  ( Dia.atop placedNodesAny placedEdges <> queryRects <> placedRegions)
  where
    (parentGraph, fullGraph) = concentrateEdges (viewGraph,fullGraphWithInfo)
    parentGraphNoLoops =  deleteLoopEdges parentGraph
    iconInfo = getIconInfo fullGraph

    layoutParams :: GV.GraphvizParams ING.Node NamedIcon (EmbedInfo Edge) ClusterT NamedIcon
    layoutParams = customLayoutParams{
      GV.fmtNode = nodeAttribute
      , GV.clusterBy = clusterNodesBy iconInfo
      , GV.fmtEdge = edgeGraphVizAttrs
      -- , GV.fmtCluster = const [ GV.GraphAttrs [GVA.Margin $ GVA.PVal $ GVA.createPoint 0.0 0.0]]
      }

    nodeAttribute :: (_, NamedIcon) -> [GV.Attribute]
    nodeAttribute (_, Named name nodeIcon) =
      [ GVA.Width diaWidth
      , GVA.Height diaHeight
      , GVA.Label $ GVA.RecordLabel recordFields
      ] where
        (diaWidth, diaHeight) = getDiagramWidthAndHeight dummyDiagram
        dummyDiagram :: Drawing b
        dummyDiagram = iconToDiagram iconInfo nodeIcon (DrawingInfo (NodeName (-1)) 0 None dummyColorStyle)

        recordFields = recordLabels iconInfo nodeIcon name

deleteLoopEdges :: ING.DynGraph gr => gr a b -> gr a b
deleteLoopEdges graph = ING.delEdges loopEdges graph where
  loopEdges = filter (uncurry (==)) $ ING.edges graph

getIconInfo :: ING.DynGraph gr => gr ( Named a) b -> IMap.IntMap a
getIconInfo fullGraph = iconInfo where
  iconInfo = IMap.fromList
            $ first nodeNameToInt . namedToTuple . snd
            <$> ING.labNodes fullGraph

renderIngSyntaxGraph :: (HasCallStack, DrawingBackend b)
  => ColorStyle
  -> (AnnotatedFGR, AnnotatedFGR) 
  -> IO (QueryableDrawing b)
renderIngSyntaxGraph colorStyle (fullGr, viweGr) 
  = renderIconGraph colorStyle fullGraph viewGraph where
    fullGraph = ING.nmap (fmap (fmap nodeToIcon)) fullGr
    viewGraph = ING.nmap (fmap (fmap nodeToIcon)) viweGr

-- graphVizEdges layoutResult = Dia.scale drawingToGraphvizScaleFactor pathDia where
--   pathDia = mconcat $ map (Dia.lwG 1 Dia.lc Dia.white . Dia.strokePath) edgePaths
--   edgePaths = map (\(_,_,_,x) -> x) $ (snd . getGraph) layoutResult