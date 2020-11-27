module FuncDefRegionInfo(getFuncDefRegionInfo) where

import qualified Data.Set as Set
import Data.List(foldl')
import Types (
  Named(..)
  , SyntaxNode(..)
  , SyntaxNodeCore(..)
  , Embedder(..)
  , NodeName
  )
import SyntaxGraph(SyntaxGraph(..))

getFuncDefRegionInfo :: SyntaxGraph -> [NodeName] -> (Set.Set NodeName, Int)
getFuncDefRegionInfo combinedGraph lambdaNames = (enclosedNodeNames, lambdaLevel) where
  innerNodes = sgNodes combinedGraph 
  lambdaLevel = getLambdaLevel innerNodes 
  enclosedNodeNames = getEnclosedNodeNames innerNodes lambdaNames  

getEnclosedNodeNames :: Set.Set (Named a) -> [NodeName] -> Set.Set NodeName
getEnclosedNodeNames innerNodes lambdaNames = enclosedNodeNames where
  allNodeNames = Set.map naName innerNodes 
  enclosedNodeNames = Set.difference allNodeNames (Set.fromList lambdaNames)

getLambdaLevel :: Set.Set (Named (Embedder SyntaxNode)) -> Int
getLambdaLevel innerNodes = level where
  allNodes = map (syntaxNodeCore . emNode . naVal) (Set.toList innerNodes) 
  funcDefLevels = [x | (FunctionValueNode _ (_, x)) <- allNodes]
  maxLevel = foldl' max 0 funcDefLevels
  level = maxLevel + 1