module FuncDefRegionInfo(getFuncDefRegionInfo, lambdaLevel, FuncDefRegionInfo) where

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

type FuncDefRegionInfo = (Set.Set NodeName, Int) 

lambdaLevel :: FuncDefRegionInfo -> Int
lambdaLevel = snd 

getFuncDefRegionInfo :: SyntaxGraph -> FuncDefRegionInfo
getFuncDefRegionInfo combinedGraph = (enclosedNodeNames, level) where
  innerNodes = sgNodes combinedGraph 
  level = getLambdaLevel innerNodes 
  enclosedNodeNames = getEnclosedNodeNames innerNodes  

getEnclosedNodeNames :: Set.Set (Named a) -> Set.Set NodeName
getEnclosedNodeNames =  Set.map _naName 

getLambdaLevel :: Set.Set (Named (Embedder SyntaxNode)) -> Int
getLambdaLevel innerNodes = level where
  allNodes = map (syntaxNodeCore . emNode . _naVal) (Set.toList innerNodes) 
  funcDefLevels = [x | (FunctionArgNode _ (_, x)) <- allNodes]
  maxLevel = foldl' max 0 funcDefLevels
  level = maxLevel + 1