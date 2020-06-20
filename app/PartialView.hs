module PartialView (
  neighborsSubgraph
) where

import qualified Data.Graph.Inductive as ING
-- import qualified Data.Graph.Inductive.Query.BFS as BFS
import Types(Named(..))

import Util(nodeNameToInt, namedToTuple)

neighborsSubgraph :: ING.DynGraph gr =>
                       Types.Named b1 -> gr a b2 -> gr a b2
neighborsSubgraph namedIcon graph = graphView where
  node = nodeNameToInt $ fst $ namedToTuple namedIcon
  nodeNeighbors = ING.neighbors graph node
  nodes = node : nodeNeighbors
  graphView = ING.subgraph nodes graph