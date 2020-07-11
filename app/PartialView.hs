module PartialView (
  neighborsSubgraph
) where

import qualified Data.Graph.Inductive as ING
-- import qualified Data.Graph.Inductive.Query.BFS as BFS
import Types(NodeName, AnnotatedFGR)

import Util(nodeNameToInt)

neighborsSubgraph :: NodeName -> AnnotatedFGR -> AnnotatedFGR
neighborsSubgraph namedIcon graph = viewGraphWithNestedNodes where
  neighborGraph = getNeighbourGraph namedIcon graph 
  viewGraphWithNestedNodes = getNestedNodesGraph neighborGraph

getNeighbourGraph  :: NodeName -> AnnotatedFGR -> AnnotatedFGR
getNeighbourGraph namedIcon graph = neighborGraph where
  node = nodeNameToInt namedIcon
  nodeNeighbors = ING.neighbors graph node
  nodes = node : nodeNeighbors
  neighborGraph = ING.subgraph nodes graph

getNestedNodesGraph neighborGraph = neighborGraph -- nestedNodesGraph where
  -- nestedNodesGraph = error $ show neighborGraph