{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, PartialTypeSignatures, ScopedTypeVariables #-}

module Rendering (
  renderDrawing
  , customLayoutParams
  , renderIngSyntaxGraph
) where

import qualified Diagrams.Prelude as Dia

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
import           Data.Maybe
import GHC.Stack(HasCallStack)

--import qualified Data.GraphViz.Types
--import Data.GraphViz.Commands

import Icons(findMaybeIconFromName)
import IconToSymbolDiagram  ( iconToDiagram
                , lambdaRegionToDiagram
                )

import SyntaxNodeToIcon(nodeToIcon)
import           Types (
  EmbedInfo(..)
  , AnnotatedGraph
  , Edge(..)
  , Drawing(..)
  , NameAndPort(..)
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
  )

import Util(nodeNameToInt, fromMaybeError, namedToTuple)
import ClusterNodesBy (
  clusterNodesBy
  , ClusterT
  )
import RenderEdges( addEdges, edgeGraphVizAttrs)

import NodePlacementMap (
  placeNode
  , getQueryRects
  ) 
-- If the inferred types for these functions becomes unweildy,
-- try using PartialTypeSignitures.

-- TODO Refactor with syntaxGraphToFglGraph in SyntaxNodeToIcon
-- TODO Make this work with nested icons now that names are not qualified.
drawingToIconGraph :: Drawing -> Gr NamedIcon (EmbedInfo Edge)
drawingToIconGraph (Drawing nodes edges) =
  mkGraph nodes labeledEdges where
    labeledEdges = fmap makeLabeledEdge (Set.toList edges)

    makeLabeledEdge :: Edge -> (NamedIcon, NamedIcon, EmbedInfo Edge)
    makeLabeledEdge e@(Edge _ (NameAndPort n1 _, NameAndPort n2 _))
      = (Named n1 (lookupInNodes n1)
        , Named n2 (lookupInNodes n2)
        , EmbedInfo Nothing e)
      where
        lookupInNodes name = fromMaybeError
                             errorString
                             (lookup name (fmap namedToTuple nodes))
          where
            errorString =
              "syntaxGraphToFglGraph edge connects to non-existent node. Node NodeName ="
              ++ show name ++ " Edge=" ++ show e

drawLambdaRegions :: forall b . SpecialBackend b Double =>
  IconInfo
  -> [(NamedIcon, SpecialDiagram b Double)]
  -> SpecialDiagram b Double
drawLambdaRegions iconInfo placedNodes
  = mconcat $ fmap (drawRegion Set.empty . fst) placedNodes
  where
    findDia :: NodeName -> SpecialDiagram b Double
    findDia n1
      = maybe mempty snd
        (find (\(Named n2 _, _) -> n1 == n2) placedNodes)

    -- Also draw the region around the icon the lambda is in.
    drawRegion :: Set.Set NodeName -> NamedIcon -> SpecialDiagram b Double
    drawRegion parentNames (Named name (Icon diagramIcon _)) = case diagramIcon of
      (FunctionDefIcon _ enclosedNames _)
        -> lambdaRegionToDiagram enclosed name where
            enclosed = findDia <$> (name : Set.toList (parentNames <> enclosedNames))
      (NestedApply _ headIcon icons)
        -> mconcat
           $ drawRegion (Set.insert name parentNames)
           <$> mapMaybe
           (findMaybeIconFromName iconInfo)
           (headIcon:icons)
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
      , GVA.RankSep [0.3]
      ]
    ]
  , GV.clusterID =  GV.Num . GV.Int --   ClusterT
  }


drawingToGraphvizScaleFactor :: Double
-- For Neato, ScaleOverlaps
--drawingToGraphvizScaleFactor = 0.08
drawingToGraphvizScaleFactor = 0.15

-- GVA.Width and GVA.Height have a minimum of 0.01
minialGVADimention :: Double
minialGVADimention = 0.01

getDiagramWidthAndHeight dummyDiagram = (diaWidth, diaHeight) where
  diaWidth = max (drawingToGraphvizScaleFactor * Dia.width dummyDiagram) minialGVADimention
  diaHeight = max (drawingToGraphvizScaleFactor * Dia.height dummyDiagram) minialGVADimention    

renderIconGraph :: forall b. SpecialBackend b Double
  => String  -- ^ Debugging information
  -> Gr (NodeInfo NamedIcon) (EmbedInfo Edge)
  -> Maybe (Gr (NodeInfo NamedIcon) (EmbedInfo Edge))
  -> IO (SpecialQDiagram b Double)
renderIconGraph debugInfo fullGraphWithInfo maybeViewGraph = do
  layoutResult <- layoutGraph' layoutParams GVA.Dot parentGraph
  let
    iconAndPositions = Map.toList $  fst $ getGraph layoutResult
    iconAndPlacedNodes :: [(NamedIcon,SpecialDiagram b Double)]
    iconAndPlacedNodes = fmap (placeNode iconInfo) iconAndPositions
    placedNodes = mconcat $ fmap snd iconAndPlacedNodes

    placedRegions = Dia.value mempty $ drawLambdaRegions iconInfo iconAndPlacedNodes

    placedEdgesAndNodes = Dia.value mempty $ addEdges debugInfo iconInfo parentGraph placedNodes

    queryRects = mconcat $ getQueryRects iconAndPlacedNodes
    -- boxesDia = mconcat $ map (Dia.lc Dia.blue $ Dia.boundingRect . snd) iconAndBoudingRect
  pure  (Dia.atop queryRects ( placedRegions <> placedEdgesAndNodes)) -- <> placedRegions <> placedEdges)
  where
    parentGraph = ING.nmap niVal $ ING.labfilter (isNothing . niParent) 
      $ fromMaybe fullGraphWithInfo maybeViewGraph
    fullGraph = ING.nmap niVal fullGraphWithInfo
    iconInfo = IMap.fromList
                $ first nodeNameToInt . namedToTuple . snd
                <$> ING.labNodes fullGraph

    layoutParams :: GV.GraphvizParams ING.Node NamedIcon (EmbedInfo Edge) ClusterT NamedIcon
    layoutParams = customLayoutParams{
      GV.fmtNode = nodeAttribute
      , GV.clusterBy = (clusterNodesBy iconInfo)
      , GV.fmtEdge = edgeGraphVizAttrs
      -- , GV.fmtCluster = (clusterAtributeList iconInfo)
      }

    nodeAttribute :: (_, NamedIcon) -> [GV.Attribute]
    nodeAttribute (_, Named _ nodeIcon) =
      [ GVA.Width diaWidth, GVA.Height diaHeight] where
        (diaWidth, diaHeight) = getDiagramWidthAndHeight dummyDiagram
        dummyDiagram :: SpecialDiagram b Double
        dummyDiagram = iconToDiagram iconInfo nodeIcon (TransformParams (NodeName (-1)) 0)
          

-- clusterAtributeList :: IconInfo -> ClusterT -> [GV.GlobalAttributes]
-- clusterAtributeList iconInfo m = [GV.GraphAttrs [ GVA.Rank rank]] where 
--   rank = case iconInfo IMap.! m of  
--     BindTextBoxIcon {} -> GVA.SinkRank
--     _ -> GVA.SameRank
--TODO add edge parameter constraint = false -- https://www.graphviz.org/doc/info/attrs.html#a:constraint 

-- | Given a Drawing, produce a Diagram complete with rotated/flipped icons and
-- lines connecting ports and icons. IO is needed for the GraphViz layout.
renderDrawing :: SpecialBackend b Double
  => String  -- ^ Debugging information
  -> Drawing
  -> IO (SpecialDiagram b Double)
renderDrawing debugInfo drawing = do
  diagram <- renderIconGraph debugInfo graph Nothing
  pure $ Dia.clearValue diagram
  where
    graph = ING.nmap (NodeInfo Nothing) . drawingToIconGraph $ drawing

renderIngSyntaxGraph :: (HasCallStack, SpecialBackend b Double)
  => String 
  -> (AnnotatedGraph Gr, Maybe (AnnotatedGraph Gr)) 
  -> IO (SpecialQDiagram b Double)
renderIngSyntaxGraph debugInfo (fullGr, viweGr) 
  = renderIconGraph debugInfo fullGraph viewGraph where
    fullGraph = ING.nmap (fmap (fmap nodeToIcon)) fullGr
    viewGraph = fmap (ING.nmap (fmap (fmap nodeToIcon))) viweGr