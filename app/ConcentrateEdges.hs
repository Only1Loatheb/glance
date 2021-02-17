{-# LANGUAGE PatternSynonyms #-}

module ConcentrateEdges(concentrateEdges) where

import qualified Data.Graph.Inductive as ING
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.IntMap as IMap

import qualified Data.Set as Set
import Data.Maybe (isJust, catMaybes, listToMaybe, isNothing)

import Diagrams.Prelude((^.))
import Util(nodeNameToInt, makeInvisibleEdge, namedToInt)

import PortConstants(pattern InputPort, pattern ResultPort)
import Types(
  SyntaxNode(..)
  , SyntaxNodeCore(..)
  , IngSyntaxGraph
  , Edge(..)
  , Port(..)
  , SgNamedNode
  , AnnotatedFGR
  , EmbedInfo(..)
  , EmbedDirection(..)
  , NodeInfo(..)
  , Embedder(..)
  , Named(..)
  , EmbedderSyntaxNode
  , NodeName(..)
  , NamedIcon(..)
  , Icon(..)
  , DiagramIcon(..)
  , EdgeOption(..)
  , naName
  , SrcRef
  , mkEmbedder
  )
-- |- should consider only drawn nodes of collapsed graph
-- concentrateEdges :: Gr NamedIcon (EmbedInfo Edge) -> Gr NamedIcon (EmbedInfo Edge)
concentrateEdges :: (Gr (NodeInfo NamedIcon) (EmbedInfo Edge), Gr (NodeInfo NamedIcon) (EmbedInfo Edge)) -> (Gr NamedIcon (EmbedInfo Edge), Gr NamedIcon (EmbedInfo Edge))
concentrateEdges (viewGraph, fullGraph) = (newViewGraph, newFullGraph) where
  nameOffset = ((+1) . snd . ING.nodeRange) fullGraph
  onlyParentsView = keepOnlyParentNodes viewGraph
  caseNodes = filter isCaseNodeLabel $ ING.labNodes onlyParentsView
  caseNamesAndSrcRefs = map getNameAndRef caseNodes

  inCase = caseNodeOrHasCaseParent fullGraph
  -- isInCase name = Set.member name (Set.fromList $ map (NodeName . fst) caseNodes) 

  viewGraphWithConcetratedEdges = ING.emap (setConcentratorNodeName nameOffset inCase) onlyParentsView

  newViewGraph = addConcetrators nameOffset caseNamesAndSrcRefs viewGraphWithConcetratedEdges
  newFullGraph = addConcetrators nameOffset caseNamesAndSrcRefs $ ING.nmap niVal fullGraph


isCaseNodeLabel :: ING.LNode NamedIcon -> Bool
isCaseNodeLabel (_, node) = isJust $ isCaseNode node

isCaseNode :: Named Icon -> Maybe ING.Node
isCaseNode (Named name (Icon NestedCaseIcon {} _)) = Just $ nodeNameToInt name
isCaseNode _ = Nothing

caseNodeOrHasCaseParent :: ING.Graph gr => gr (NodeInfo (Named Icon)) b -> ING.Node -> Maybe ING.Node
caseNodeOrHasCaseParent fullGraph name = case ING.lab fullGraph name of
  Just (NodeInfo Nothing node) -> isCaseNode node
  Just (NodeInfo (Just parent) node) -> listToMaybe $ catMaybes [isCaseNode node, caseNodeOrHasCaseParent fullGraph parent]
  Nothing -> Nothing

getNameAndRef :: (a, Named Icon) -> (NodeName, SrcRef)
getNameAndRef (_, Named name (Icon _ ref)) = (name, ref) 

setConcentratorNodeName :: Int -> (ING.Node -> Maybe ING.Node) -> EmbedInfo Edge -> EmbedInfo Edge
setConcentratorNodeName nameOffset isInCase edge@(EmbedInfo info (Edge DrawAndNotConstraint con@(from, to) )) = newEdge where
  isEdgeFromCaseToCase = (isInCase (namedToInt from ) ,  isInCase (namedToInt to))
  newEdge = case isEdgeFromCaseToCase of
    (Just fromCaseParent, Just toCaseParent) | fromCaseParent /= toCaseParent 
      -> EmbedInfo info (Edge (DrawThrough $ concetratorParams nameOffset fromCaseParent toCaseParent) con) where
    _ -> edge
setConcentratorNodeName _ _ edge = edge

concetratorParams :: Int -> Int -> Int -> (Named Port, Named Port)
concetratorParams nameOffset fromCaseParent toCaseParent = params where
  params = (concentratorNamedPort nameOffset (NodeName fromCaseParent), concentratorNamedPort nameOffset (NodeName toCaseParent))

concentratorName :: Int -> NodeName -> NodeName
concentratorName nameOffset = NodeName . (+ nameOffset) . nodeNameToInt

concentratorNamedPort :: Int -> NodeName -> Named Port
concentratorNamedPort nameOffset name = Named (concentratorName nameOffset name) ResultPort

addConcetrators :: Int -> [(NodeName,SrcRef)] -> Gr NamedIcon (EmbedInfo Edge) -> Gr NamedIcon (EmbedInfo Edge)
addConcetrators nameOffset caseNames graph = newGraph where
  newGraph = (ING.insEdges concentratorEdges . ING.insNodes concentratorNodes) graph
  concentratorNodes = map (concentratorIcon nameOffset) caseNames
  concentratorEdges = map (concentratorEdge nameOffset) caseNames
  

concentratorEdge :: Int -> (NodeName, SrcRef) -> ING.LEdge (EmbedInfo Edge)
concentratorEdge nameOffset (name,_) = (nodeNameToInt fromName, nodeNameToInt toName, edge) where
  edge =  EmbedInfo Nothing (makeInvisibleEdge 1 connection)
  connection = (concentratorNamedPort nameOffset fromName, Named toName InputPort)
  fromName = concentratorName nameOffset name
  toName = name 

concentratorIcon :: Int -> (NodeName, SrcRef) -> ING.LNode NamedIcon
concentratorIcon nameOffset (name,ref) = (nodeNameToInt iconName, Named iconName (Icon Concentrator ref)) where
  iconName = concentratorName nameOffset name

keepOnlyParentNodes :: Gr (NodeInfo c) b -> Gr c b
keepOnlyParentNodes viewGraph = ING.nmap niVal $ ING.labfilter (isNothing . niParent) viewGraph