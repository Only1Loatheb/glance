{-# LANGUAGE NoMonomorphismRestriction, TupleSections, PatternSynonyms #-}

module SyntaxGraph(
    SyntaxGraph(..)
  , patternName
  , syntaxGraphFromNodes
  , syntaxGraphFromNodesEdges
  , bindsToSyntaxGraph
  , graphAndRefToGraph
  , getUniqueName
  , getUniqueString
  , combineExpressions
  , namesInPattern
  , lookupReference
  , deleteBindings
  , makeEdgesAndDeleteBindings
  , makeNotConstraintEdgesAndDeleteBindings
  , makeEdges
  , makeEdges'
  -- , makeOutputEdgesAndSinks
  , asBindGraphZipper
  , EvalContext
  , GraphAndRef(..)
  , Reference
  , SgSink(..)
  , SgBind
  , grNamePortToGrRef
  , initialIdState
  , lookupInEmbeddingMap
  , edgeForRefPortIsSource
  , edgeForRefPortIsNotSource
  , syntaxGraphFromEdges
  , makeAsBindGraph
  , graphsToComponents
  , combineExpresionsIsSource
) where

import Control.Monad.State ( State, state )
import Diagrams.Prelude((<>))
import           Data.Either( partitionEithers)
import           Data.Maybe(
  fromMaybe
  , mapMaybe
  , catMaybes
  )
import qualified Data.Set as Set
import qualified Data.StringMap as SMap
import qualified Data.IntMap as IMap


import           PortConstants(
  inputPort
  , resultPort
  , argumentPorts
  , argPortsConst
  , multiIfValuePorts
  , multiIfBoolPorts
  , resultPortsConst
  , pattern PatternValuePortConst
  )

import           Types(
  Labeled(..)
  , NameAndPort(..)
  , IDState(..)
  , Edge(..)
  , SyntaxNode(..)
  , NodeName(..)
  , SgNamedNode
  , LikeApplyFlavor(..)
  , CaseOrMultiIfTag(..)
  , Named(..)
  , EdgeOption(..)
  , mkEmbedder
  , Connection(..)
  )
import Util(
  makeSimpleEdge
  , makeNotConstraintEdge
  , makeInvisibleEdge
  , nameAndPort
  , justName
  )
import StringSymbols(defaultPatternNameStr)

{-# ANN module "HLint: ignore Use list comprehension" #-}

-- OVERVIEW --
-- This module has the core functions and data types used by SimpSyntaxToSyntaxGraph.
-- This module also contains most/all of the translation functions that
-- do not require Language.Haskell.Exts.

type Reference = Either String NameAndPort

type EvalContext = Set.Set String

type SgBind = (SMap.Key, Reference)

data SgSink = SgSink String NameAndPort deriving (Eq, Ord, Show)

-- | A SyntaxGraph is an abstract representation of Haskell syntax. SyntaxGraphs
-- are generated from the Haskell syntax tree and are used to generate Drawings.
data SyntaxGraph = SyntaxGraph {
  sgNodes :: (Set.Set SgNamedNode),
  sgEdges :: (Set.Set Edge),
  sgSinks :: (Set.Set SgSink),
  sgBinds :: (SMap.StringMap Reference ), -- Reference -> Reference 
  -- sgEmbedMap keeps track of nodes embedded in other nodes. If (child, parent)
  -- is in the Map, then child is embedded inside parent.
  sgEmbedMap :: IMap.IntMap NodeName -- NodeName -> NodeName
  } deriving (Show, Eq)

instance Semigroup SyntaxGraph where
  (<>)
    (SyntaxGraph icons1 edges1 sinks1 sources1 map1)
    (SyntaxGraph icons2 edges2 sinks2 sources2 map2)
    = SyntaxGraph
      (Set.union icons1 icons2)
      (Set.union edges1 edges2)
      (Set.union sinks1 sinks2)
      (SMap.union sources1 sources2)
      (IMap.union map1 map2)

instance Monoid SyntaxGraph where
  mempty = SyntaxGraph Set.empty Set.empty Set.empty SMap.empty mempty
  mappend = (<>)

data GraphAndRef = GraphAndRef SyntaxGraph Reference

lookupInEmbeddingMap :: NodeName -> IMap.IntMap NodeName -> NodeName
lookupInEmbeddingMap origName eMap = lookupHelper origName where
  lookupHelper :: NodeName -> NodeName
  lookupHelper name@(NodeName nameInt) = case IMap.lookup nameInt eMap of
    Nothing -> name
    Just parent -> if parent == origName
      then error $ "lookupInEmbeddingMap: Found cycle. Node = "
           ++ show origName ++ "\nEmbedding Map = " ++ show eMap
      else lookupHelper parent

-- BEGIN Constructors and Destructors

syntaxGraphFromNodes :: Set.Set SgNamedNode -> SyntaxGraph
syntaxGraphFromNodes icons = SyntaxGraph icons Set.empty Set.empty SMap.empty mempty

syntaxGraphFromNodesEdges :: Set.Set SgNamedNode -> Set.Set Edge -> SyntaxGraph
syntaxGraphFromNodesEdges icons edges = SyntaxGraph icons edges Set.empty SMap.empty mempty

bindsToSyntaxGraph :: SMap.StringMap Reference -> SyntaxGraph
bindsToSyntaxGraph binds = SyntaxGraph Set.empty Set.empty Set.empty binds mempty

sinksToSyntaxGraph :: Set.Set SgSink -> SyntaxGraph
sinksToSyntaxGraph sinks = SyntaxGraph Set.empty Set.empty sinks SMap.empty mempty

syntaxGraphFromEdges :: Set.Set Edge -> SyntaxGraph
syntaxGraphFromEdges edges = SyntaxGraph Set.empty edges mempty SMap.empty mempty

graphAndRefToGraph :: GraphAndRef -> SyntaxGraph
graphAndRefToGraph (GraphAndRef g _) = g

graphAndRefToRef :: GraphAndRef -> Reference
graphAndRefToRef (GraphAndRef _ r) = r

graphToTuple ::
  SyntaxGraph
  -> (Set.Set SgNamedNode, (Set.Set Edge), (Set.Set SgSink), (SMap.StringMap Reference), IMap.IntMap NodeName)
graphToTuple (SyntaxGraph a b c d e) = (a, b, c, d, e)

graphsToComponents ::
  [SyntaxGraph]
  -> (Set.Set SgNamedNode, (Set.Set Edge), (Set.Set SgSink), (SMap.StringMap Reference), IMap.IntMap NodeName)
graphsToComponents graphs = graphToTuple $ mconcat graphs

-- END Constructors and Destructors

-- BEGIN IDState
initialIdState :: IDState
initialIdState = IDState 0

getId :: State IDState Int
getId = state incrementer where
  incrementer (IDState x) = (x, IDState checkedIncrement) where
    checkedIncrement = if x /= maxBound
      then x + 1
      else error "getId: the ID state has overflowed."

getUniqueName :: State IDState NodeName
getUniqueName = fmap NodeName getId

-- TODO Should getUniqueString prepend an illegal character?
getUniqueString :: String -> State IDState String
getUniqueString base = fmap ((base ++). show) getId
-- END IDState

-- | Make a syntax graph that has the bindings for a list of "as pattern" (@)
-- names.
makeAsBindGraph :: Reference -> [Maybe String] -> SyntaxGraph
makeAsBindGraph ref asNames
  = bindsToSyntaxGraph (SMap.fromList (mapMaybe makeBind asNames))
  where
    makeBind mName = case mName of
      Nothing -> Nothing
      Just asName -> Just $ (asName, ref)
    

combineExpressions :: Bool -> [(GraphAndRef, NameAndPort)] -> SyntaxGraph
combineExpressions isSource portExpPairs
  = if isSource
    then mconcat $ fmap (combineExpresionsIsSource makeSimpleEdge) portExpPairs 
    else mconcat $ fmap (combineExpresionsNotIsSource makeSimpleEdge) portExpPairs 

combineExpresionsIsSource :: (Connection-> Edge) -> (GraphAndRef, NameAndPort) -> SyntaxGraph
combineExpresionsIsSource edgeConstructor (GraphAndRef graph ref, port) 
  = graph <> edgeForRefPortIsSource edgeConstructor ref port

combineExpresionsNotIsSource :: (Connection-> Edge) -> (GraphAndRef, NameAndPort) -> SyntaxGraph
combineExpresionsNotIsSource edgeConstructor (GraphAndRef graph ref, port) 
  = graph <> edgeForRefPortIsNotSource edgeConstructor ref port
    

edgeForRefPortIsSource :: (Connection-> Edge) -> Reference -> NameAndPort -> SyntaxGraph
edgeForRefPortIsSource edgeConstructor ref port = case ref of
      Left str -> bindsToSyntaxGraph $ SMap.singleton str (Right port)
      Right resPort -> syntaxGraphFromEdges $ Set.singleton  (edgeConstructor connection)
        where
          connection = (port, resPort)

edgeForRefPortIsNotSource :: (Connection-> Edge) -> Reference -> NameAndPort -> SyntaxGraph
edgeForRefPortIsNotSource edgeConstructor ref port = case ref of
      Left str -> sinksToSyntaxGraph $ Set.singleton (SgSink str port)
      Right resPort -> syntaxGraphFromEdges $ Set.singleton  (edgeConstructor connection)
        where
          connection = (resPort, port)




grNamePortToGrRef :: (SyntaxGraph, NameAndPort) -> GraphAndRef
grNamePortToGrRef (graph, np) = GraphAndRef graph (Right np)

namesInPattern :: (GraphAndRef, Maybe String) -> EvalContext
namesInPattern (graphAndRef, mName) = case mName of
  Nothing -> otherNames
  Just n -> Set.insert n  otherNames
  where
    otherNames = namesInPatternHelper graphAndRef

    namesInPatternHelper :: GraphAndRef -> EvalContext
    namesInPatternHelper (GraphAndRef graph ref) = case ref of
      Left str -> Set.singleton str
      Right _ -> Set.fromList $ SMap.keys (sgBinds graph)

-- | Recursivly find the matching reference in a list of bindings.
-- TODO: Might want to present some indication if there is a reference cycle.
lookupReference :: (SMap.StringMap Reference) -> Reference -> Reference
lookupReference _ ref@(Right _) = ref
lookupReference bindings originalRef = lookupReference' originalRef where
  
  lookupReference' ref@(Right _) = ref
  lookupReference' (Left s) = ref where 
    foundRef = SMap.lookup s  bindings
    refMayBeCausingCycle = fromMaybe originalRef foundRef
    ref = if refMayBeCausingCycle == originalRef 
      then originalRef
      else lookupReference' refMayBeCausingCycle

deleteBindings :: SyntaxGraph -> SyntaxGraph
deleteBindings (SyntaxGraph a b c _ e) = SyntaxGraph a b c SMap.empty e

makeEdgesAndDeleteBindings :: SyntaxGraph -> SyntaxGraph
makeEdgesAndDeleteBindings = deleteBindings . (makeEdges' makeSimpleEdge)

makeNotConstraintEdgesAndDeleteBindings :: SyntaxGraph -> SyntaxGraph
makeNotConstraintEdgesAndDeleteBindings = deleteBindings . (makeEdges' makeNotConstraintEdge)

makeEdges :: SyntaxGraph -> SyntaxGraph
makeEdges = makeEdges' makeSimpleEdge

makeEdges' :: (Connection -> Edge) -> SyntaxGraph -> SyntaxGraph
makeEdges' egdeConstructor (SyntaxGraph icons edges sinks bindings eMap) = newGraph where
  (newSinks, newEdges) = makeEdgesCore egdeConstructor sinks bindings
  newGraph = SyntaxGraph icons (newEdges <> edges) newSinks bindings eMap

makeEdgesCore :: (Connection -> Edge)-> (Set.Set SgSink) -> (SMap.StringMap Reference) -> ((Set.Set SgSink), (Set.Set Edge))
makeEdgesCore egdeConstructor sinks bindings = (Set.fromList newSinks,Set.fromList newEdge) where
  -- TODO check if set->list->set gives optimal performance
  (newSinks, newEdge) = partitionEithers $ fmap renameOrMakeEdge (Set.toList sinks)
  renameOrMakeEdge :: SgSink -> Either SgSink Edge
  renameOrMakeEdge orig@(SgSink s destPort)
    = case SMap.lookup s bindings of
        Just ref -> case lookupReference bindings ref of
          Right sourcePort -> Right $ egdeConstructor (sourcePort, destPort)
          Left newStr -> Left $ SgSink newStr destPort
        Nothing -> Left orig

patternName :: (GraphAndRef, Maybe String) -> String
patternName (GraphAndRef _ ref, mStr) = fromMaybe
  (case ref of
    Left str -> str
    Right _ -> defaultPatternNameStr
  )
  mStr

-- makeOutputEdgesAndSinks :: Reference
--   -> Set.Set Edge -> NameAndPort -> (Set.Set Edge, Set.Set SgSink)
-- makeOutputEdgesAndSinks rhsRef patternEdges returnPort = case rhsRef of
--   Left s -> (patternEdges, Set.singleton (SgSink s returnPort))
--   Right rhsPort -> (Set.insert (makeSimpleEdge (rhsPort, returnPort)) patternEdges, mempty)

asBindGraphZipper :: Maybe String -> NameAndPort -> SyntaxGraph
asBindGraphZipper asName nameNPort = makeAsBindGraph (Right nameNPort) [asName]