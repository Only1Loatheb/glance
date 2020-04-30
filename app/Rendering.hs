{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, PartialTypeSignatures, ScopedTypeVariables #-}

module Rendering (
  renderDrawing,
  customLayoutParams,
  renderIngSyntaxGraph
) where

import qualified Diagrams.Prelude as DIA
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
            , TransformParams(..))

import Util(nodeNameToInt, fromMaybeError, namedToTuple)

-- If the inferred types for these functions becomes unweildy,
-- try using PartialTypeSignitures.

-- CONSTANT
graphvizScaleFactor :: (Fractional a) => a

-- For Neato
graphvizScaleFactor = 0.12

-- For Fdp
--scaleFactor = 0.09

--scaleFactor = 0.04

drawingToGraphvizScaleFactor :: Fractional a => a
-- For Neato, ScaleOverlaps
--drawingToGraphvizScaleFactor = 0.08

-- For Neato, PrismOverlap
drawingToGraphvizScaleFactor = 0.15

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
makeEdge iconInfo graph lEdge
  = connectMaybePorts iconInfo graph lEdge

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

getPositionOfNamed origDia n = case DIA.lookupName n origDia of
  --Nothing -> DIA.r2 (0, 0)--error "Name does not exist!"
  Nothing -> Nothing-- error $ "Name does not exist! name=" <> show n <> "\neInfo=" <> show eInfo
  Just subDia -> Just $ DIA.location subDia

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
      Named lambdaName lambdaIcon@(LambdaIcon _ _ enclosedNames)
        -> lambdaRegionToDiagram enclosed lambdaIcon lambdaName where
            enclosed = findDia <$> Set.toList (parentNames <> enclosedNames)
      Named parentName (NestedApply _ headIcon icons)
        -> mconcat
           $ drawRegion (Set.insert parentName parentNames)
           <$> mapMaybe
           (findMaybeIconFromName iconInfo)
           (headIcon:icons)
      _ -> mempty


-- TODO place nodes from top to bottom 
-- placeNode :: SpecialBackend b Double 
--   => IconInfo
--   -> Icon
--   -> SpecialQDiagram b Double
placeNode :: SpecialBackend b Double
  => IconInfo 
  -> (NamedIcon, P2 Double) 
  -> (NamedIcon, SpecialQDiagram b Double)
placeNode namedIcons (key@(Named name icon), targetPosition)
  = (key, place transformedDia diaPosition) where
      origDia = iconToDiagram
                namedIcons
                icon
                (TransformParams name 0)
      transformedDia = centerXY origDia
      diaPosition = graphvizScaleFactor *^ targetPosition

customLayoutParams :: GV.GraphvizParams n v e () v
customLayoutParams = GV.defaultParams{
  GV.globalAttributes = [
    GV.NodeAttrs [GVA.Shape GVA.BoxShape]
    --GV.NodeAttrs [GVA.Shape GVA.Circle]
    , GV.GraphAttrs
      [
      --GVA.Overlap GVA.KeepOverlaps,
      --GVA.Overlap GVA.ScaleOverlaps,
      GVA.Overlap $ GVA.PrismOverlap (Just 5000),
      GVA.Splines GVA.NoEdges,
      GVA.OverlapScaling 8,
      --GVA.OverlapScaling 4,
      GVA.OverlapShrink True
      ]
    ],
  GV.fmtEdge = const [GV.arrowTo GV.noArrow]
  }

renderIconGraph :: forall b.
  SpecialBackend b Double =>
  String  -- ^ Debugging information
  -> Gr (NodeInfo NamedIcon) (EmbedInfo Edge)
  -> IO (SpecialQDiagram b Double)
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
    placedNodesAndRegions = placedNodes <> placedRegions
    edges = addEdges debugInfo iconInfo parentGraph placedNodesAndRegions
    placedRegions = drawLambdaRegions iconInfo placedNodeList
  pure (edges <> placedNodesAndRegions)
  where
    parentGraph
      = ING.nmap niVal $ ING.labfilter (isNothing . niParent) fullGraphWithInfo
    fullGraph = ING.nmap niVal fullGraphWithInfo
    iconInfo = IMap.fromList
                 $ first nodeNameToInt . namedToTuple . snd
                 <$> ING.labNodes fullGraph

    layoutParams :: GV.GraphvizParams Int NamedIcon e () NamedIcon
    --layoutParams :: GV.GraphvizParams Int l el Int l
    layoutParams = customLayoutParams{
      GV.fmtNode = nodeAttribute
      }
    nodeAttribute :: (Int, NamedIcon) -> [GV.Attribute]
    nodeAttribute (_, Named _ nodeIcon) =
      -- [GVA.Width circleDiameter, GVA.Height circleDiameter]
      [GVA.Width diaWidth, GVA.Height diaHeight] where
        -- This type annotation (:: SpecialQDiagram b n) requires Scoped Typed
        -- Variables, which only works if the function's
        -- type signiture has "forall b e."
        dia :: SpecialQDiagram b Double
        dia = iconToDiagram
              iconInfo
              nodeIcon
              (TransformParams (NodeName (-1)) 0)
        
        diaWidth = max (drawingToGraphvizScaleFactor * width dia) minialDiaDimention
        diaHeight = max (drawingToGraphvizScaleFactor * height dia) minialDiaDimention
        -- circleDiameter = maximum [diaWidth, diaHeight, minialDiaDimention]

-- GVA.Width and GVA.Height have a minimum of 0.01
minialDiaDimention :: Double
minialDiaDimention = 0.01

-- | Given a Drawing, produce a Diagram complete with rotated/flipped icons and
-- lines connecting ports and icons. IO is needed for the GraphViz layout.
renderDrawing :: SpecialBackend b Double
  => String  -- ^ Debugging information
  -> Drawing
  -> IO (SpecialQDiagram b Double)
renderDrawing debugInfo drawing
  = renderIconGraph debugInfo graph
  where
    graph = ING.nmap (NodeInfo Nothing) . drawingToIconGraph $ drawing

renderIngSyntaxGraph :: (HasCallStack, SpecialBackend b Double)
  => String -> AnnotatedGraph Gr -> IO (SpecialQDiagram b Double)
renderIngSyntaxGraph debugInfo gr
  = renderIconGraph debugInfo
    $ ING.nmap (fmap (fmap nodeToIcon)) gr
