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
import           Data.Maybe
import GHC.Stack(HasCallStack)

--import qualified Data.GraphViz.Types
--import Data.GraphViz.Commands

import Icons(findMaybeIconFromName)
import IconToDiagram( iconToDiagram, lambdaRegionToDiagram, lambdaRegionPadding)

import SyntaxNodeToIcon(nodeToIcon)
import           Types (
  EmbedInfo(..)
  , Edge(..)
  , SpecialDiagram
  , SpecialBackend
  , NodeName(..)
  , NamedIcon
  , Icon(..)
  , DiagramIcon(..)
  , NodeInfo(..)
  , IconInfo
  , Named(..)
  , TransformParams(..)
  , SpecialQDiagram
  , AnnotatedFGR
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
import DrawingColors (ColorStyle,dummyColorStyle)
-- If the inferred types for these functions becomes unweildy,
-- try using PartialTypeSignitures.

drawLambdaRegions :: forall b . SpecialBackend b Double =>
  ColorStyle Double 
  -> IconInfo
  -> [(NamedIcon, SpecialDiagram b Double)]
  -> SpecialDiagram b Double
drawLambdaRegions colorStyle iconInfo placedNodes
  = mconcat $ fmap (drawRegion Set.empty . fst) placedNodes
  where
    findDia :: NodeName -> SpecialDiagram b Double
    findDia n1
      = maybe mempty snd
        (find (\(Named n2 _, _) -> n1 == n2) placedNodes)

    -- Also draw the region around the icon the lambda is in.
    -- Consult CollapseGraph to find out where FunctionDefIcon can be nested 
    drawRegion :: Set.Set NodeName -> NamedIcon -> SpecialDiagram b Double
    drawRegion parentNames (Named name (Icon diagramIcon _)) = case diagramIcon of
      (FunctionDefIcon _ (enclosedNames,level) maybeEmbededNode)
        -> thisRegionDiagram <> innerRegionDiagram where
          thisRegionDiagram = lambdaRegionToDiagram colorStyle enclosed name level
          enclosed = findDia <$> (name : Set.toList (parentNames <> enclosedNames))
          innerRegionDiagram = case findMaybeIconFromName iconInfo maybeEmbededNode of
            Nothing -> mempty
            Just foundIcon -> drawRegion (Set.insert name parentNames) foundIcon
      _ -> mempty

customLayoutParams :: GV.GraphvizParams ING.Node v e ClusterT v
customLayoutParams = GV.defaultParams{
  GV.globalAttributes = [
    GV.NodeAttrs [GVA.Shape GVA.BoxShape]
    , GV.EdgeAttrs [GVA.MinLen 1]
    --GV.NodeAttrs [GVA.Shape GVA.Circle]
    , GV.GraphAttrs
      [
      --GVA.Overlap GVA.KeepOverlaps,
      --GVA.Overlap GVA.ScaleOverlaps,
      GVA.Overlap $ GVA.PrismOverlap (Just 5000),
      GVA.Splines GVA.Curved,
      GVA.OverlapScaling 8,
      --GVA.OverlapScaling 4,
      GVA.OverlapShrink True
      , GVA.ClusterRank GVA.Local
      , GVA.RankSep [1.2 * lambdaRegionPadding * drawingToGraphvizScaleFactor]
      , GVA.NodeSep $ 0.5 * lambdaRegionPadding * drawingToGraphvizScaleFactor
      ]
    ]
  , GV.clusterID =  GV.Num . GV.Int --   ClusterT
  }


drawingToGraphvizScaleFactor :: Double
-- For Neato, ScaleOverlaps
--drawingToGraphvizScaleFactor = 0.08
-- has to be set acording to drawings
drawingToGraphvizScaleFactor = 0.13

-- GVA.Width and GVA.Height have a minimum of 0.01
minialGVADimention :: Double
minialGVADimention = 0.01

getDiagramWidthAndHeight :: forall b. SpecialBackend b Double => SpecialDiagram b Double -> (Double, Double)
getDiagramWidthAndHeight dummyDiagram = (diaWidth, diaHeight) where
  diaWidth = max (drawingToGraphvizScaleFactor * Dia.width dummyDiagram) minialGVADimention
  diaHeight = max (drawingToGraphvizScaleFactor * Dia.height dummyDiagram) minialGVADimention    

renderIconGraph :: forall b. SpecialBackend b Double
  => ColorStyle Double
  -> Gr (NodeInfo NamedIcon) (EmbedInfo Edge)
  -> Gr (NodeInfo NamedIcon) (EmbedInfo Edge)
  -> IO (SpecialQDiagram b Double)
renderIconGraph colorStyle fullGraphWithInfo viewGraph = do
  layoutResult <- layoutGraph' layoutParams GVA.Dot parentGraph
  let
    iconAndPositions = Map.toList $  fst $ getGraph layoutResult
    iconAndPlacedNodes :: [(NamedIcon,SpecialDiagram b Double)]
    iconAndPlacedNodes = map (placeNode iconInfo colorStyle drawingToGraphvizScaleFactor) iconAndPositions
    placedNodes = mconcat $ fmap snd iconAndPlacedNodes

    placedRegions = Dia.value mempty $ drawLambdaRegions colorStyle iconInfo iconAndPlacedNodes
    placedEdges = Dia.value mempty $ makeEdges colorStyle iconInfo parentGraph placedNodes
    placedNodesAny = Dia.value mempty placedNodes

    queryRects = mconcat $ getQueryRects iconAndPlacedNodes
    -- boxesDia = mconcat $ map (Dia.lc Dia.blue $ Dia.boundingRect . snd) iconAndBoudingRect
  pure  ( Dia.atop placedNodesAny placedEdges <> queryRects <> placedRegions )
  where
    parentGraph = ING.nmap niVal $ ING.labfilter (isNothing . niParent) viewGraph
    fullGraph = ING.nmap niVal fullGraphWithInfo
    iconInfo = IMap.fromList
                $ first nodeNameToInt . namedToTuple . snd
                <$> ING.labNodes fullGraph

    layoutParams :: GV.GraphvizParams ING.Node NamedIcon (EmbedInfo Edge) ClusterT NamedIcon
    layoutParams = customLayoutParams{
      GV.fmtNode = nodeAttribute
      , GV.clusterBy = clusterNodesBy iconInfo
      , GV.fmtEdge = edgeGraphVizAttrs iconInfo
      -- , GV.fmtCluster = (clusterAtributeList iconInfo)
      }

    nodeAttribute :: (_, NamedIcon) -> [GV.Attribute]
    nodeAttribute (_, Named _ nodeIcon) =
      [ GVA.Width diaWidth, GVA.Height diaHeight] where
        (diaWidth, diaHeight) = getDiagramWidthAndHeight dummyDiagram
        dummyDiagram :: SpecialDiagram b Double
        dummyDiagram = iconToDiagram iconInfo dummyColorStyle nodeIcon (TransformParams (NodeName (-1)) 0)

          
--TODO add edge parameter constraint = false -- https://www.graphviz.org/doc/info/attrs.html#a:constraint 

renderIngSyntaxGraph :: (HasCallStack, SpecialBackend b Double)
  => ColorStyle Double
  -> (AnnotatedFGR, AnnotatedFGR) 
  -> IO (SpecialQDiagram b Double)
renderIngSyntaxGraph colorStyle (fullGr, viweGr) 
  = renderIconGraph colorStyle fullGraph viewGraph where
    fullGraph = ING.nmap (fmap (fmap nodeToIcon)) fullGr
    viewGraph = ING.nmap (fmap (fmap nodeToIcon)) viweGr