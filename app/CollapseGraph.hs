{-# LANGUAGE MultiWayIf #-}
module CollapseGraph(
  syntaxGraphToCollapsedGraph
  , syntaxGraphToLessCollapsedGraph
  -- for tests:
  , annotateGraph
  , syntaxGraphToFglGraph
  , collapseAnnotatedGraph
  ) where

import qualified Data.Graph.Inductive.PatriciaTree as FGR
import qualified Data.Graph.Inductive as ING
import Data.List(foldl', find)
import qualified Data.Set as Set
import Data.Tuple(swap)
import GHC.Stack(HasCallStack)

import PortConstants(
  isInputPort
  , isPatternUnpackingPort
  , isArgPort
  , isQualPort
  , isResultPort
  )
import Types(
  SyntaxNode(..)
  , SyntaxNodeCore(..)
  , IngSyntaxGraph
  , Edge(..)
  , Port(..)
  , NameAndPort(..)
  , SgNamedNode
  , AnnotatedGraph
  , EmbedInfo(..)
  , EmbedDirection(..)
  , NodeInfo(..)
  , Embedder(..)
  , Named(..)
  , EmbedderSyntaxNode
  , NodeName(..)
  , CaseFlavor(..)
  , naVal
  )
import SyntaxGraph(SyntaxGraph(..), lookupInEmbeddingMap)

import StringSymbols(isTempLabel)
import Util(
  nodeNameToInt
  , fromMaybeError
  )
import Diagrams.Prelude

type IsEmbeddableTest = (ParentType -> SyntaxNode -> Port -> Port -> Bool) 
data ParentType = ApplyParent
                | CaseParent
                | LambdaParent String
                | PatternParent
                | ListCompParent
                | ListLitParent
                | NotAParent
  deriving (Eq, Show)

-- Helper functions

parentAndChild :: EmbedDirection
               -> (a, a) -- ^ (from, to)
               -> (a, a) -- ^ (parent, child)
parentAndChild embedDirection
  = case embedDirection of
      EdEmbedTo -> id
      EdEmbedFrom -> swap

-- End helper functions
-- START annotateGraph --

-- TODO Use pattern synonyms here
-- | A isSyntaxNodeEmbeddable if it can be collapsed into another node
isSyntaxNodeEmbeddable :: IsEmbeddableTest
isSyntaxNodeEmbeddable parentType (SyntaxNode syntaxNode _) parentPort childPort
  = case (parentType, syntaxNode) of
    (ApplyParent, ApplyNode {}) -> parentPortNotResult
    (ApplyParent, LiteralNode {}) -> parentPortNotResult

    (ListLitParent, ApplyNode {}) -> parentPortNotResult
    (ListLitParent, LiteralNode {}) -> parentPortNotResult

    (LambdaParent fname, ApplyNode {}) -> lambdaEmbeddable fname
    (LambdaParent fname, CaseNode {}) -> lambdaEmbeddable fname
    (LambdaParent fname, LiteralNode {}) -> lambdaEmbeddable fname
    (LambdaParent fname, ListLitNode {}) -> lambdaEmbeddable fname
    (LambdaParent fname, FunctionValueNode {}) -> lambdaEmbeddable fname

    (CaseParent, CaseResultNode {}) -> isParentQualPort
    (CaseParent, LiteralNode {}) -> parentPortNotResult
    (CaseParent, PatternNode {}) -> caseEmbeddable || parentPortIsArg && isPatternUnpackingPort childPort
    (CaseParent, CaseNode {}) -> False 
    (CaseParent, _) -> caseEmbeddable
    
    (PatternParent, PatternNode {}) -> isPatternUnpackingPort childPort && parentPortNotInput
    (PatternParent, LiteralNode {}) -> True
    (PatternParent, _) -> isPatternUnpackingPort parentPort && childPortIsResult 

    (ListCompParent, ApplyNode {}) -> listCompEmbeddable
    -- (ListCompParent, PatternNode {}) -> parentPortNotInput
    (ListCompParent, LiteralNode {}) -> listCompEmbeddable
    (ListCompParent, ListLitNode {}) -> listCompEmbeddable
    (ListCompParent, ListCompNode {}) -> parentPortNotInput && parentPortIsArg && childPortIsResult

    _ -> False
  where
    isParentQualPort = isQualPort parentPort

    parentPortIsInput = isInputPort parentPort
    parentPortIsArg = isArgPort parentPort
    childPortIsResult = isResultPort childPort

    parentPortNotInput = not parentPortIsInput
    parentPortNotResult = not $ isResultPort parentPort

    listCompEmbeddable = isParentQualPort
      || parentPortIsArg && childPortIsResult

    lambdaEmbeddable fname = parentPortIsInput && isTempLabel fname && childPortIsResult
    
    caseEmbeddable = parentPortNotResult && parentPortNotInput && childPortIsResult
      || parentPortIsInput && childPortIsResult

doesSyntaxNodeHaveToBeEmbeded :: IsEmbeddableTest
doesSyntaxNodeHaveToBeEmbeded parentType (SyntaxNode syntaxNode _) parentPort _childPort
  = case (parentType, syntaxNode) of
    (ApplyParent, ApplyNode {}) -> parentPortNotResult
    (ApplyParent, LiteralNode {}) -> parentPortNotResult

    (ListLitParent, ApplyNode {}) -> parentPortNotResult
    (ListLitParent, LiteralNode {}) -> parentPortNotResult

    (CaseParent, CaseResultNode {}) -> True
    _ -> False
  where
    parentPortNotResult = not $ isResultPort parentPort

parentTypeForNode :: SyntaxNode -> ParentType
parentTypeForNode (SyntaxNode n _) = case n of
  ApplyNode {} -> ApplyParent
  CaseNode {} -> CaseParent
  FunctionValueNode fname -> LambdaParent fname
  PatternNode {} -> PatternParent
  ListLitNode {} -> ListLitParent
  ListCompNode {} -> ListCompParent
  _ -> NotAParent

lookupSyntaxNode :: ING.Graph gr =>
  IngSyntaxGraph gr -> ING.Node -> Maybe EmbedderSyntaxNode
lookupSyntaxNode gr node =  over _Just _naVal (ING.lab gr node)

lookupParentType :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> ParentType
lookupParentType graph node
  = maybe NotAParent parentTypeForNode $ emNode <$> lookupSyntaxNode graph node

{-# ANN edgeIsSingular "HLint: ignore Redundant bracket" #-}
edgeIsSingular :: ING.Graph gr => gr a Edge -> ING.Node -> Edge -> Bool
edgeIsSingular graph node edge = numEdges <= 1 where
  (childNamePort, _) = edgeConnection edge
  edgeLabels = filter
               (childNamePort ==)
               ((fst . edgeConnection . snd) <$> ING.lsuc graph node)
  numEdges = length edgeLabels

parentCanEmbedChild :: ING.Graph gr =>
  IsEmbeddableTest
  -> IngSyntaxGraph gr -> ING.Node -> ING.Node -> Edge -> EmbedDirection -> Bool
parentCanEmbedChild embedingTest graph parent child edge embedDirection
  = case lookupSyntaxNode graph child of
      Nothing -> False
      Just childSyntaxNode ->
        edgeIsSingular graph child edge
        && embedingTest
        parentType
        (emNode childSyntaxNode)
        parentPort
        childPort
        where
          parentType = lookupParentType graph parent
          (Named _ fromPort, Named _ toPort) = edgeConnection edge
          (parentPort, childPort)
            = parentAndChild embedDirection (fromPort, toPort)

findEmbedDir :: ING.Graph gr => 
  IsEmbeddableTest
  -> IngSyntaxGraph gr
  -> ING.Node
  -> ING.Node
  -> Edge
  -> Maybe EmbedDirection
findEmbedDir embedingTest gr fromNode toNode e = if
  | parentCanEmbedChild embedingTest gr fromNode toNode e EdEmbedTo
    -> Just EdEmbedTo
  | parentCanEmbedChild embedingTest gr toNode fromNode e EdEmbedFrom
    -> Just EdEmbedFrom
  | otherwise -> Nothing


annotateGraph' :: ING.DynGraph gr => 
  IsEmbeddableTest
  -> IngSyntaxGraph gr -> gr SgNamedNode (EmbedInfo Edge)
annotateGraph' embedingTest gr = ING.gmap edgeMapper gr
  where
    edgeMapper :: ING.Context SgNamedNode Edge
               -> ING.Context SgNamedNode (EmbedInfo Edge)
    edgeMapper (inEdges, node, nodeLabel, outEdges)
      = (getInEmbedInfo node inEdges
        , node
        , nodeLabel
        , getOutEmbedInfo node outEdges)
    getInEmbedInfo toNode
      = fmap (\(e, fromNode)
               -> (EmbedInfo (findEmbedDir embedingTest gr fromNode toNode e) e, fromNode))
    getOutEmbedInfo fromNode
     = fmap (\(e, toNode)
              -> (EmbedInfo (findEmbedDir embedingTest gr fromNode toNode e) e, toNode))

annotateGraph :: IngSyntaxGraph ING.Gr -> ING.Gr SgNamedNode (EmbedInfo Edge)
annotateGraph = annotateGraph' isSyntaxNodeEmbeddable

annotateGraphLessCollapsed :: IngSyntaxGraph ING.Gr -> ING.Gr  SgNamedNode (EmbedInfo Edge)
annotateGraphLessCollapsed = annotateGraph' doesSyntaxNodeHaveToBeEmbeded
-- END annotateGraph --
-- START collapseAnnotatedGraph --

findEdgeLabel :: ING.Graph gr => gr a b -> ING.Node -> ING.Node -> Maybe b
findEdgeLabel graph node1 node2 = fmap fst matchingEdges where
  labelledEdges = ING.lneighbors graph node1
  matchingEdges = find ((== node2) . snd) labelledEdges

-- | Replace the a node's label
changeNodeLabel :: ING.DynGraph gr => ING.Node -> a -> gr a b -> gr a b
changeNodeLabel node newLabel graph = case ING.match node graph of
  (Just (inEdges, _, _, outEdges), restOfTheGraph)
    -> (inEdges, node, newLabel, outEdges) ING.& restOfTheGraph
  (Nothing, _) -> graph

addChildToNodeLabel ::
  (NodeName, Edge) -> EmbedderSyntaxNode -> EmbedderSyntaxNode
addChildToNodeLabel child (Embedder existingNodes oldSyntaxNode)
  = Embedder (Set.insert child existingNodes) oldSyntaxNode

-- | Change the node label of the parent to be nested.
embedChildSyntaxNode :: ING.DynGraph gr =>
  ING.Node -> ING.Node -> AnnotatedGraph gr -> AnnotatedGraph gr
embedChildSyntaxNode parentNode childNode oldGraph = newGraph
  where
    mChildAndEdge =
      (,) <$> ING.lab oldGraph childNode
      <*> findEdgeLabel oldGraph parentNode childNode
    newGraph = case ING.lab oldGraph parentNode of
      Nothing -> error "embedChildSyntaxNode: parentNode not found"
      Just (NodeInfo isChild oldNodeLabel) ->
        -- TODO Refactor with the Maybe Monad?
        case mChildAndEdge of
          Nothing -> error "embedChildSyntaxNode: childNode not found."
          Just (NodeInfo _ childNodeLab, EmbedInfo _ edge)
            -> changeNodeLabel
               childNode
               (NodeInfo (Just parentNode) childNodeLab)
               $ changeNodeLabel parentNode newNodeLabel oldGraph
            where
              Named nodeName oldSyntaxNode = oldNodeLabel
              newSyntaxNode = addChildToNodeLabel
                              (_naName childNodeLab, edge)
                              oldSyntaxNode
              newNodeLabel = NodeInfo isChild (Named nodeName newSyntaxNode)

collapseEdge :: (HasCallStack, ING.DynGraph gr)
             => AnnotatedGraph gr
             -> ING.LEdge (EmbedInfo Edge)
             -> AnnotatedGraph gr
collapseEdge oldGraph lEdge@(fromNode, toNode, EmbedInfo mEmbedDir _)
  = case mEmbedDir of
      Nothing -> oldGraph
      Just embedDir -> ING.delLEdge lEdge childEmbeddedGraph
        where
          (parentNode, childNode) = parentAndChild embedDir (fromNode, toNode)
          childEmbeddedGraph
            = embedChildSyntaxNode parentNode childNode oldGraph

mapEdges :: (ING.Graph gr1, ING.Graph gr2)
  => (ING.LEdge b1 -> ING.LEdge b2)
  -> gr1 a b1
  -> gr2 a b2
mapEdges f gr = ING.mkGraph nodes mappedEdges
  where
    nodes = ING.labNodes gr
    mappedEdges = f <$> ING.labEdges gr

findRootAncestor :: ING.Graph gr
  => gr (NodeInfo a) b -> ING.Node -> ING.Node
findRootAncestor gr node =
  let nodeLab = fromMaybeError
        "findRootAncestor: node does not exist"
        (ING.lab gr node)
  in
    case niParent nodeLab of
      Nothing -> node
      Just parentNode -> findRootAncestor gr parentNode

-- Note: modifying the edges could probably be eliminated if the algorithms in
-- Rendering were re-written to us the node's parent.
-- | For all of the graph edges, this function moves edge to from and to nodes
-- of the edge to be root (the parents's parent parent etc.) of the edge's
-- from and to nodes.
moveEdges :: (ING.Graph gr1, ING.Graph gr2)
  => gr1 (NodeInfo a) b -> gr2 (NodeInfo a) b
moveEdges gr = mapEdges moveEdge gr
  where
    moveEdge (fromNode, toNode, label) = (newFrom, newTo, label)
      where
        newFrom = findRootAncestor gr fromNode
        newTo = findRootAncestor gr toNode

collapseAnnotatedGraph :: (HasCallStack, ING.DynGraph gr)
                       => gr SgNamedNode (EmbedInfo Edge)
                       -> AnnotatedGraph gr
collapseAnnotatedGraph origGraph = moveEdges newGraph
  where
    defaultNodeInfoGraph = ING.nmap (NodeInfo Nothing) origGraph
   -- TODO Check that there are no embedded edges left.
    newGraph = foldl' collapseEdge defaultNodeInfoGraph (ING.labEdges origGraph)

-- Exported functions
syntaxGraphToFglGraph :: SyntaxGraph -> FGR.Gr SgNamedNode Edge
syntaxGraphToFglGraph (SyntaxGraph nodes edges _ _ eMap) =
  ING.mkGraph labeledNodes labeledEdges where
    labeledNodes = fmap makeLabeledNode (Set.toList nodes)
    labeledEdges = fmap makeLabeledEdge (Set.toList edges)

    makeLabeledEdge e@(Edge _ (Named name1 _, Named name2 _)) =
      (nodeNameToInt $ lookupInEmbeddingMap name1 eMap
      , nodeNameToInt $ lookupInEmbeddingMap name2 eMap
      , e)
    
    makeLabeledNode :: SgNamedNode -> ING.LNode SgNamedNode
    makeLabeledNode namedNode@(Named (NodeName name) _) = (name, namedNode)

syntaxGraphToCollapsedGraph :: SyntaxGraph -> AnnotatedGraph FGR.Gr
syntaxGraphToCollapsedGraph
  = collapseAnnotatedGraph . annotateGraph . syntaxGraphToFglGraph

syntaxGraphToLessCollapsedGraph :: SyntaxGraph -> AnnotatedGraph FGR.Gr
syntaxGraphToLessCollapsedGraph
  = collapseAnnotatedGraph . annotateGraphLessCollapsed . syntaxGraphToFglGraph

