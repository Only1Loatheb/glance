module PartialView (
  neighborsSubgraph
) where

import qualified Data.Graph.Inductive as ING
-- import qualified Data.Graph.Inductive.Query.BFS as BFS
import Types(
  NodeName
  , AnnotatedFGR
  )
import Util(nodeNameToInt)

neighborsSubgraph  :: NodeName -> AnnotatedFGR -> AnnotatedFGR
neighborsSubgraph namedIcon graph = nodeNeighborhoodGraph where
  node = nodeNameToInt namedIcon
  nodeNeighborhoodGraph = ING.subgraph (node : ING.neighbors graph node) graph