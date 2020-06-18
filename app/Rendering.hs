{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, PartialTypeSignatures, ScopedTypeVariables #-}

module Rendering (
  renderDrawing
  , customLayoutParams
  , renderIngSyntaxGraph
) where

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
import RenderEdges( addEdges, edgeGraphVizAttrs)

import NodePlacementMap (
  getDiagramWidthAndHeight
  , placeNode
  , makePointToIcon
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
  -> [(NamedIcon, SpecialQDiagram b Double)]
  -> SpecialQDiagram b Double
drawLambdaRegions iconInfo placedNodes
  = mconcat $ fmap (drawRegion Set.empty . fst) placedNodes
  where
    findDia :: NodeName -> SpecialQDiagram b Double
    findDia n1
      = maybe mempty snd
        (find (\(Named n2 _, _) -> n1 == n2) placedNodes)

    -- Also draw the region around the icon the lambda is in.
    drawRegion :: Set.Set NodeName -> NamedIcon -> SpecialQDiagram b Double
    drawRegion parentNames icon = case icon of
      Named lambdaName lambdaIcon@(FunctionDefIcon _ enclosedNames _)
        -> lambdaRegionToDiagram enclosed lambdaName where
            enclosed = findDia <$> (lambdaName : Set.toList (parentNames <> enclosedNames))
      Named parentName (NestedApply _ headIcon icons)
        -> mconcat
           $ drawRegion (Set.insert parentName parentNames)
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

getBoundingRects layoutResult = []

renderIconGraph :: forall b. SpecialBackend b Double
  => String  -- ^ Debugging information
  -> Gr (NodeInfo NamedIcon) (EmbedInfo Edge)
  -> IO (SpecialQDiagram b Double, (Dia.P2 Double -> Maybe Icon))
renderIconGraph debugInfo fullGraphWithInfo = do
    -- graph = ING.nmap niVal fullGraphWithInfo
  layoutResult <- layoutGraph' layoutParams GVA.Dot parentGraph
  --  layoutResult <- layoutGraph' layoutParams GVA.Fdp graph
  let
    positionMap = fst $ getGraph layoutResult
    -- rotationMap = rotateNodes iconInfo positionMap parentGraph
    placedNodeList :: [(NamedIcon,SpecialQDiagram b Double)]
    placedNodeList = fmap (placeNode iconInfo) (Map.toList positionMap)
    placedNodes = mconcat $ fmap snd placedNodeList
    placedRegions = drawLambdaRegions iconInfo placedNodeList
    placedNodesAndRegions = placedNodes <> placedRegions
    edges = addEdges debugInfo iconInfo parentGraph placedNodesAndRegions

    iconAndBoudingRect = getBoundingRects layoutResult
    pointToIcon = makePointToIcon  iconAndBoudingRect
  pure (placedNodesAndRegions <> edges, pointToIcon)
  where
    parentGraph
      = ING.nmap niVal $ ING.labfilter (isNothing . niParent) fullGraphWithInfo
    fullGraph = ING.nmap niVal fullGraphWithInfo
    iconInfo = IMap.fromList
                $ first nodeNameToInt . namedToTuple . snd
                <$> ING.labNodes fullGraph

    layoutParams :: GV.GraphvizParams ING.Node NamedIcon (EmbedInfo Edge) ClusterT NamedIcon
    --layoutParams :: GV.GraphvizParams Int l el Int l
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
        dummyDiagram :: SpecialQDiagram b Double
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
  -> IO (SpecialQDiagram b Double)
renderDrawing debugInfo drawing = do
  (diagram, _) <- renderIconGraph debugInfo graph
  pure diagram
  where
    graph = ING.nmap (NodeInfo Nothing) . drawingToIconGraph $ drawing

renderIngSyntaxGraph :: (HasCallStack, SpecialBackend b Double)
  => String 
  -> AnnotatedGraph Gr 
  -> IO (SpecialQDiagram b Double, (Dia.P2 Double -> Maybe Icon))
renderIngSyntaxGraph debugInfo gr 
  = renderIconGraph debugInfo graph where
    graph = ING.nmap (fmap (fmap nodeToIcon)) gr