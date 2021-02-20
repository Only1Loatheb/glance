{-# LANGUAGE PatternSynonyms #-}

module ConcentrateEdges(concentrateEdges) where

import qualified Data.Graph.Inductive as ING
import Data.Graph.Inductive.PatriciaTree (Gr)

import Data.Maybe (isJust, catMaybes, listToMaybe, isNothing)

import Util(nodeNameToInt, makeInvisibleEdge, namedToInt)

import PortConstants(pattern InputPort, pattern ResultPort, caseValuePorts)
import Types(
  Edge(..)
  , Port(..)
  , EmbedInfo(..)
  , NodeInfo(..)
  , Named(..)
  , NodeName(..)
  , NamedIcon
  , Icon(..)
  , DiagramIcon(..)
  , EdgeOption(..)
  , SrcRef
  )

data CaseInfo = CaseInfo {
  ciName :: NodeName
  , ciRef :: SrcRef
  }   

concentratorAboveNodeInRanks :: Int
concentratorAboveNodeInRanks = 2

-- |- should consider only drawn nodes of collapsed graph
-- concentrateEdges :: Gr NamedIcon (EmbedInfo Edge) -> Gr NamedIcon (EmbedInfo Edge)
concentrateEdges :: (Gr (NodeInfo NamedIcon) (EmbedInfo Edge), Gr (NodeInfo NamedIcon) (EmbedInfo Edge)) -> (Gr NamedIcon (EmbedInfo Edge), Gr NamedIcon (EmbedInfo Edge), Int)
concentrateEdges (viewGraph, fullGraph) = (newViewGraph, newFullGraph, nameOffset) where
  nameOffset = ((+1) . snd . ING.nodeRange) fullGraph
  onlyParentsView = keepOnlyParentNodes viewGraph
  
  caseNamesAndSrcRefs = [CaseInfo name ref | (_,Named name (Icon (NestedCaseIcon _ _ args) ref)) <- ING.labNodes onlyParentsView]

  inCase = caseNodeOrHasCaseParent fullGraph

  viewGraphWithconcentratedEdges = ING.emap (setConcentratorNodeName nameOffset inCase) onlyParentsView

  newViewGraph = addconcentrators nameOffset caseNamesAndSrcRefs viewGraphWithconcentratedEdges
  newFullGraph = addconcentrators nameOffset caseNamesAndSrcRefs $ ING.nmap niVal fullGraph

isCaseNode :: Named Icon -> Maybe ING.Node
isCaseNode (Named name (Icon NestedCaseIcon {} _)) = Just $ nodeNameToInt name
isCaseNode _ = Nothing

caseNodeOrHasCaseParent :: ING.Graph gr => gr (NodeInfo (Named Icon)) b -> ING.Node -> Maybe ING.Node
caseNodeOrHasCaseParent fullGraph name = case ING.lab fullGraph name of
  Just (NodeInfo Nothing node) -> isCaseNode node
  Just (NodeInfo (Just parent) node) -> listToMaybe $ catMaybes [isCaseNode node, caseNodeOrHasCaseParent fullGraph parent]
  Nothing -> Nothing

setConcentratorNodeName :: Int -> (ING.Node -> Maybe ING.Node) -> EmbedInfo Edge -> EmbedInfo Edge
setConcentratorNodeName nameOffset isInCase edge@(EmbedInfo info (Edge DrawAndNotConstraint con@(from, to) )) = newEdge where
  isEdgeFromCaseToCase = (isInCase (namedToInt from ) ,  isInCase (namedToInt to))
  newEdge = case isEdgeFromCaseToCase of
    (Just fromCaseParent, Just toCaseParent) | fromCaseParent /= toCaseParent 
      -> EmbedInfo info (Edge (DrawThrough $ concentratorParams nameOffset fromCaseParent toCaseParent) con) where
    _ -> edge
setConcentratorNodeName _ _ edge = edge

concentratorParams :: Int -> Int -> Int -> (Named Port, Named Port)
concentratorParams nameOffset fromCaseParent toCaseParent = params where
  params = (concentratorNamedPort nameOffset (NodeName fromCaseParent), concentratorNamedPort nameOffset (NodeName toCaseParent))

concentratorName :: Int -> NodeName -> NodeName
concentratorName nameOffset = NodeName . (+ nameOffset) . nodeNameToInt

concentratorNamedPort :: Int -> NodeName -> Named Port
concentratorNamedPort nameOffset name = Named (concentratorName nameOffset name) ResultPort

addconcentrators :: Int -> [CaseInfo] -> Gr NamedIcon (EmbedInfo Edge) -> Gr NamedIcon (EmbedInfo Edge)
addconcentrators nameOffset caseNames graph = newGraph where
  newGraph = (ING.insEdges concentratorEdges . ING.insNodes concentratorNodes) graph
  concentratorNodes = map (concentratorIcon nameOffset) caseNames
  concentratorEdges = map (concentratorEdge nameOffset) caseNames
  

concentratorEdge :: Int -> CaseInfo -> ING.LEdge (EmbedInfo Edge)
concentratorEdge nameOffset (CaseInfo toName _) = (nodeNameToInt fromName, nodeNameToInt toName, edge) where
  edge =  EmbedInfo Nothing (makeInvisibleEdge concentratorAboveNodeInRanks connection)
  connection = (concentratorNamedPort nameOffset fromName, Named toName ResultPort)
  fromName = concentratorName nameOffset toName

concentratorIcon :: Int -> CaseInfo -> ING.LNode NamedIcon
concentratorIcon nameOffset (CaseInfo name ref) = (nodeNameToInt iconName, Named iconName (Icon Concentrator ref)) where
  iconName = concentratorName nameOffset name

keepOnlyParentNodes :: Gr (NodeInfo c) b -> Gr c b
keepOnlyParentNodes viewGraph = ING.nmap niVal $ ING.labfilter (isNothing . niParent) viewGraph