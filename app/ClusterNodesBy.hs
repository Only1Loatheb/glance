module ClusterNodesBy (
  clusterNodesBy
  , ClusterT
  ) where

import qualified Data.GraphViz as GV
import qualified Data.IntMap as IMap
import qualified Data.IntSet as ISet
import qualified Data.Set as Set

import qualified Data.Graph.Inductive as ING

import Types(
  NamedIcon
  , Icon(..)
  , DiagramIcon(..)
  , IconInfo
  )

import Util(nodeNameToInt)

type ClusterT = Int

clusterNodesBy ::
  IconInfo
  -> ((ING.Node , NamedIcon) -> GV.NodeCluster ClusterT  (ING.Node , NamedIcon))
clusterNodesBy iconInfo  = clusterBy where
  clusterBy :: (ING.Node , NamedIcon) -> GV.NodeCluster ClusterT  (ING.Node , NamedIcon)
  clusterBy (nodeName, namedIcon) = 
    GV.C (IMap.findWithDefault nodeName nodeName clusterMap) 
    $ GV.N (nodeName,namedIcon)
    -- Also draw the region around the icon the lambda is in.
  clusterMap :: IMap.IntMap ClusterT
  clusterMap = foldr (combineClusterMaps . iconClusterMap) IMap.empty (IMap.toList iconInfo)

iconClusterMap :: (IMap.Key, Icon) -> IMap.IntMap ClusterT
iconClusterMap (name, Icon (FunctionArgIcon _ (nodesInside,_)) _) = lambdaClusterMap where
  lambdaClusterMap = IMap.fromAscList $ map (\x -> (nodeNameToInt x, -name)) (Set.toAscList nodesInside)
iconClusterMap _ = IMap.empty -- TODO other nested nodes

combineClusterMaps :: IMap.IntMap ClusterT -> IMap.IntMap ClusterT -> IMap.IntMap ClusterT
combineClusterMaps map1 map2 = newMap where
  keySet1 = IMap.keysSet map1 
  keySet2 = IMap.keysSet map2
  newMap
    | ISet.disjoint keySet1 keySet2 = IMap.union map1 map2
    | keySet1 `ISet.isProperSubsetOf` keySet2 = IMap.union map1 map2
    | keySet2 `ISet.isProperSubsetOf` keySet1 = IMap.union map2 map1
    | otherwise = error "combineClusterMaps"