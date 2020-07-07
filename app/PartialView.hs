module PartialView (
  neighborsSubgraph
) where

import qualified Data.Graph.Inductive as ING
-- import qualified Data.Graph.Inductive.Query.BFS as BFS
import Types(NodeName, AnnotatedFGR)

import Util(nodeNameToInt)

neighborsSubgraph :: NodeName -> AnnotatedFGR -> AnnotatedFGR
neighborsSubgraph namedIcon graph = graphView where
  node = nodeNameToInt namedIcon
  nodeNeighbors = ING.neighbors graph node
  nodes = node : nodeNeighbors
  graphView = ING.subgraph nodes graph