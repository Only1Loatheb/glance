module ClusterNodesBy (
  clusterNodesBy
  , ClusterT
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
import qualified Data.IntMap as IMap
import qualified Data.IntSet as ISet
import qualified Data.Set as Set

import qualified Data.Graph.Inductive as ING


import SyntaxNodeToIcon(nodeToIcon)
import Types(EmbedInfo(..), AnnotatedGraph, Edge(..)
            , Drawing(..), NameAndPort(..)
            , SpecialQDiagram, SpecialBackend, SpecialNum, NodeName(..)
            , NamedIcon, Icon(..), NodeInfo(..), IconInfo
            , Named(..)
            , TransformParams(..))

import Util(nodeNameToInt, fromMaybeError, namedToTuple)

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
  clusterMap = foldr combineClusterMaps IMap.empty $ map iconClusterMap (IMap.toList iconInfo)

iconClusterMap :: (IMap.Key, Icon) -> IMap.IntMap ClusterT
iconClusterMap (name, (FunctionDefIcon _ nodesInside _)) = lambdaClusterMap where
  lambdaClusterMap = IMap.fromAscList $ map (\x -> (nodeNameToInt x,name)) (Set.toList nodesInside)
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