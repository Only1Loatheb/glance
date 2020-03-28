{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, PartialTypeSignatures, ScopedTypeVariables #-}

module Rendering (
  renderDrawing,
  customLayoutParams,
  renderIngSyntaxGraph
) where

import qualified Diagrams.Prelude as DIA
import Diagrams.Prelude(toName, shaftStyle, global, arrowShaft, noTail
                       , arrowTail, arrowHead, scale, r2, bezier3
                       , fromSegments, Angle, P2, V2, Point, ArrowOpts, N
                       , TrailLike, V, height, width, (*^), reflectX, rotate
                       , centerXY, place
                       , rect, dashingG, lwG, lightgreen, lc, centerPoint
                       , moveTo, turn, (@@), unitX, signedAngleBetween, (.-.)
                       , applyAll, angleV, rad, (^.), angleBetween, (.>)
                       , connectOutside', connect', with, (%~), lengths, (^+^)
                       , (.~), scaleX, (*^), unitX, unitY)
import Diagrams.TwoD.GraphViz(mkGraph, getGraph, layoutGraph')
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GVA
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Arrow(first)
import Data.Function(on)
import qualified Data.Graph.Inductive as ING
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List(find, minimumBy)
import Data.Maybe(isNothing, fromMaybe, mapMaybe)
import Data.Typeable(Typeable)
import GHC.Stack(HasCallStack)

--import qualified Data.GraphViz.Types
--import Data.GraphViz.Commands

import Icons(findIconFromName)
import Symbols(colorScheme, iconToDiagram, defaultLineWidth, ColorStyle(..), sizeUnit)
import TranslateCore(nodeToIcon)
import Types(EmbedInfo(..), AnnotatedGraph, Edge(..)
            , Drawing(..), NameAndPort(..)
            , SpecialQDiagram, SpecialBackend, SpecialNum, NodeName(..)
            , Port(..), NamedIcon, Icon(..), NodeInfo(..), IconInfo
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

-- TODO Refactor with syntaxGraphToFglGraph in TranslateCore
-- TODO Make this work with nested icons now that names are not qualified.
drawingToIconGraph :: Drawing -> Gr NamedIcon (EmbedInfo Edge)
drawingToIconGraph (Drawing nodes edges) =
  mkGraph nodes labeledEdges where
    labeledEdges = fmap makeLabeledEdge edges

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

-- https://archives.haskell.org/projects.haskell.org/diagrams/doc/arrow.html

bezierShaft form to = fromSegments [bezier3 offsetToControl1 offsetToControl2 offsetToEnd] where
  scaleFactor = sizeUnit * 10
  offsetToEnd = to .-. form
  offsetToControl1 = scale scaleFactor (-unitY)
  offsetToControl2 = (scale scaleFactor unitY) ^+^ offsetToEnd

getArrowOpts :: (RealFloat n, Typeable n) =>
  (Maybe (Point V2 n),Maybe (Point V2 n))
  -> NameAndPort
  -> (ArrowOpts n, DIA.Colour Double)
getArrowOpts
  (formPoint, toPoint)
  (NameAndPort (NodeName nodeNum) mPort)
  = (arrowOptions, shaftColor)
  where
    edgeColors = edgeListC colorScheme
    Port portNum = fromMaybe (Port 0) mPort
    namePortHash = mod (portNum + (503 * nodeNum)) (length edgeColors)
    shaftColor = edgeColors !! namePortHash
    form = fromMaybe (DIA.p2 (0.0,0.0)) formPoint
    to = fromMaybe (DIA.p2  (0.0,-1.0)) toPoint
    arrowOptions =
      -- arrowHead .~ DIA.noHead
      arrowHead .~ DIA.tri
      $ DIA.headStyle %~ DIA.fc shaftColor
      $ arrowTail .~ noTail
      $ arrowShaft .~ bezierShaft form to
      -- TODO Don't use a magic number for lengths (headLength and tailLength)
      $ lengths .~ global 0.5
      $ with

-- | Given an Edge, return a transformation on Diagrams that will draw a line.
connectMaybePorts :: SpecialBackend b n =>
  EmbedInfo Edge
  -> SpecialQDiagram b n
  -> SpecialQDiagram b n
connectMaybePorts
  (EmbedInfo embedDir
    (Edge
      _
      (fromNamePort@(NameAndPort name0 mPort1)
      , NameAndPort name1 mPort2)))
  origDia
  -- In order to give arrows a "shadow" effect, draw a thicker semi-transparent
  -- line shaft the same color as the background underneath the normal line
  -- shaft.
  = -- if DIA.location (DIA.lookupName qPort0 origDia) == DIA.location (DIA.lookupName qPort1 origDia
  -- if nameToPoint qPort0 == nameToPoint qPort1
  --   then error "connectMaybePorts: fromNamePort equals toNamePort!"
  case pointsTheSame of
    Nothing -> origDia
    Just True -> origDia
    _ ->
      (connectFunc normalOpts qPort0 qPort1
       . connectFunc arrOptsShadow qPort0 qPort1) origDia
  where
    pointsTheSame = do
      p0 <- nameToPoint qPort0
      p1 <- nameToPoint qPort1
      return $ p0 == p1
    nameToPoint n = case DIA.lookupName n origDia of
      --Nothing -> DIA.r2 (0, 0)--error "Name does not exist!"
      Nothing -> Nothing-- error $ "Name does not exist! name=" <> show n <> "\neInfo=" <> show eInfo
      Just subDia -> Just $ DIA.location subDia


    lineWidth = 2 * defaultLineWidth
    
    from = nameToPoint qPort0
    to = nameToPoint qPort1
    (baseArrOpts, shaftCol) = getArrowOpts (from,to) fromNamePort
    -- TODO Use a color from the color scheme for un-embedded shafts.
    shaftCol' = if isNothing embedDir then shaftCol else DIA.lime
    normalOpts = (shaftStyle %~ (lwG lineWidth . lc shaftCol'))
                 baseArrOpts
    arrOptsShadow = (shaftStyle
                     %~ (lwG (1.9 * lineWidth)
                         . DIA.lcA
                          $ DIA.withOpacity (backgroundC colorScheme) 0.5))
                    baseArrOpts
    (connectFunc, qPort0, qPort1) = case (mPort1, mPort2) of
      (Just port0, Just port1) -> (connect', name0 .> port0, name1 .> port1)
      (Nothing, Just port1) -> (connectOutside', toName name0, name1 .> port1)
      (Just port0, Nothing) -> (connectOutside', name0 .> port0, toName name1)
      (_, _) -> (connectOutside', toName name0, toName name1)


-- START addEdges --

makeEdge :: (HasCallStack, SpecialBackend b n) 
  => ING.LEdge (EmbedInfo Edge)
  -> SpecialQDiagram b n
  -> SpecialQDiagram b n
makeEdge 
  (_node0, _node1, edge@(EmbedInfo _ (Edge _ (_namePort0, _namePort1))))
  = connectMaybePorts edge


-- | addEdges draws the edges underneath the nodes.
addEdges :: (HasCallStack, SpecialBackend b n, ING.Graph gr) =>
  String  -- ^ Debugging information
  -> IconInfo
  -> gr NamedIcon (EmbedInfo Edge)
  -> SpecialQDiagram b n
  -> SpecialQDiagram b n
addEdges _debugInfo _iconInfo graph = applyAll connections
  where
    connections = makeEdge  <$> ING.labEdges graph

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
      Named lambdaName (LambdaIcon _ _ enclosedNames)
        -> regionRect enclosed  where
            enclosedWithoutParent = Set.delete lambdaName enclosedNames 
            enclosed =  findDia <$> Set.toList {-enclosedNames -} enclosedWithoutParent
      Named parentName (NestedApply _ headIcon icons)
        -> mconcat
           $ drawRegion (Set.insert parentName parentNames)
           <$> mapMaybe
           (fmap (findIconFromName iconInfo))
           (headIcon:icons)
      _ -> mempty

    regionRect :: forall b . SpecialBackend b Double
      => [SpecialQDiagram b Double]
      -> SpecialQDiagram b Double
    regionRect enclosedDiagarms
      = moveTo (centerPoint combinedDia) coloredContentsRect
      where
        combinedDia = mconcat enclosedDiagarms
        rectPadding = 2 * sizeUnit
        contentsRect = dashingG [0.7 * sizeUnit, 0.3 * sizeUnit] 0
                       $ rect
                       (rectPadding + width combinedDia)
                       (rectPadding + height combinedDia)
        coloredContentsRect = lc lightgreen (lwG defaultLineWidth contentsRect)
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
                (TransformParams name 0 False (0 @@ turn))
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
      GVA.Splines GVA.LineEdges,
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
  layoutResult <- layoutGraph' layoutParams GVA.Neato parentGraph
  --  layoutResult <- layoutGraph' layoutParams GVA.Fdp graph
  let
    positionMap = fst $ getGraph layoutResult
    -- rotationMap = rotateNodes iconInfo positionMap parentGraph
    placedNodeList :: [(NamedIcon,SpecialQDiagram b Double)]
    placedNodeList = fmap (placeNode iconInfo) (Map.toList positionMap)
    placedNodes = mconcat $ fmap snd placedNodeList
    edges = addEdges debugInfo iconInfo parentGraph placedNodes
    placedRegions = drawLambdaRegions iconInfo placedNodeList
  pure (placedNodes <> edges <> placedRegions)
  where
    parentGraph
      = ING.nmap niVal $ ING.labfilter (isNothing . niParent) fullGraphWithInfo
    fullGraph = ING.nmap niVal fullGraphWithInfo
    iconInfo = IM.fromList
                 $ first nodeNameToInt . namedToTuple . snd
                 <$> ING.labNodes fullGraph

    layoutParams :: GV.GraphvizParams Int NamedIcon e () NamedIcon
    --layoutParams :: GV.GraphvizParams Int l el Int l
    layoutParams = customLayoutParams{
      GV.fmtNode = nodeAttribute
      }
    nodeAttribute :: (Int, NamedIcon) -> [GV.Attribute]
    nodeAttribute (_, Named _ nodeIcon) =
      -- GVA.Width and GVA.Height have a minimum of 0.01
      --[GVA.Width diaWidth, GVA.Height diaHeight]
      [GVA.Width circleDiameter, GVA.Height circleDiameter]
      where
        -- This type annotation (:: SpecialQDiagram b n) requires Scoped Typed
        -- Variables, which only works if the function's
        -- type signiture has "forall b e."
        dia :: SpecialQDiagram b Double
        dia = iconToDiagram
              iconInfo
              nodeIcon
              (TransformParams (NodeName (-1)) 0 False mempty)

        diaWidth = drawingToGraphvizScaleFactor * width dia
        diaHeight = drawingToGraphvizScaleFactor * height dia
        circleDiameter' = max diaWidth diaHeight
        circleDiameter
          = if circleDiameter' <= 0.01
            then error ("circleDiameter too small: " ++ show circleDiameter')
            else circleDiameter'

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
