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
  , refToNamePort
  , deleteBindings
  , makeEdges
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
  , edgeFromPortToRef
  , edgeFromRefToPort
  , syntaxGraphFromEdges
  , makeAsBindGraph
  , graphsToComponents
  , combineFromPortToGraph
  , graphAndRefToRef
  , makeEdgesKeepBindings
  , deleteBindingsWithRef
  , combineFromGraphToPort
) where

import Control.Monad.State ( State, state )
import Diagrams.Prelude((<>))
import           Data.Either( partitionEithers, fromRight)
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
  , Reference
  , EvalContext
  , SgBind
  , SgSink(..)
  , SyntaxGraph(..)
  , GraphAndRef(..)
  )
import Util(
  makeSimpleEdge
  , makeNotConstraintEdge
  , makeInvisibleEdge
  , nameAndPort
  )
import StringSymbols(defaultPatternNameStr)

{-# ANN module "HLint: ignore Use list comprehension" #-}

-- OVERVIEW --
-- This module has the core functions and data types used by SimpSyntaxToSyntaxGraph.
-- This module also contains most/all of the translation functions that
-- do not require Language.Haskell.Exts.

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

-- BEGIN Pattern
asBindGraphZipper :: Maybe String -> NameAndPort -> SyntaxGraph
asBindGraphZipper asName nameNPort = makeAsBindGraph (Right nameNPort) [asName]

-- | Make a syntax graph that has the bindings for a list of "as pattern" (@)
-- names.
makeAsBindGraph :: Reference -> [Maybe String] -> SyntaxGraph
makeAsBindGraph ref asNames
  = bindsToSyntaxGraph (SMap.fromList (mapMaybe makeBind asNames))
  where
    makeBind mName = case mName of
      Nothing -> Nothing
      Just asName -> Just $ (asName, ref)

patternName :: (GraphAndRef, Maybe String) -> String
patternName (GraphAndRef _ ref, mStr) = fromMaybe
  (case ref of
    Left str -> str
    Right _ -> defaultPatternNameStr
  )
  mStr

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

-- END Pattern
-- BEGIN make edges
combineExpressions :: Bool -> [(GraphAndRef, NameAndPort)] -> SyntaxGraph
combineExpressions isFromPortToGraph portExpPairs
  = if isFromPortToGraph
    then mconcat $ fmap (uncurry $ combineFromPortToGraph makeSimpleEdge) portExpPairs 
    else mconcat $ fmap (uncurry $ combineFromGraphToPort makeSimpleEdge) portExpPairs 

combineFromPortToGraph :: (Connection-> Edge) -> GraphAndRef -> NameAndPort -> SyntaxGraph
combineFromPortToGraph edgeConstructor (GraphAndRef graph ref) port 
  = graph <> edgeFromPortToRef edgeConstructor ref port

combineFromGraphToPort :: (Connection-> Edge) -> GraphAndRef -> NameAndPort -> SyntaxGraph
combineFromGraphToPort edgeConstructor (GraphAndRef graph ref) port 
  = graph <> edgeFromRefToPort edgeConstructor ref port
    

edgeFromPortToRef :: (Connection-> Edge) -> Reference -> NameAndPort -> SyntaxGraph
edgeFromPortToRef edgeConstructor ref port = case ref of
      Left str -> bindsToSyntaxGraph $ SMap.singleton str (Right port)
      Right refPort -> syntaxGraphFromEdges $ Set.singleton  (edgeConstructor connection)
        where
          connection = (port, refPort)

edgeFromRefToPort :: (Connection-> Edge) -> Reference -> NameAndPort -> SyntaxGraph
edgeFromRefToPort edgeConstructor ref port = case ref of
      Left str -> sinksToSyntaxGraph $ Set.singleton (SgSink str port)
      Right refPort -> syntaxGraphFromEdges $ Set.singleton  (edgeConstructor connection)
        where
          connection = (refPort, port)
-- END make edges
grNamePortToGrRef :: (SyntaxGraph, NameAndPort) -> GraphAndRef
grNamePortToGrRef (graph, np) = GraphAndRef graph (Right np)

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

refToNamePort :: Reference -> NameAndPort 
refToNamePort = fromRight (error "Not nameAndPort")

deleteBindings :: SyntaxGraph -> SyntaxGraph
deleteBindings (SyntaxGraph a b c _ e) = SyntaxGraph a b c SMap.empty e


deleteBindingsWithRef :: GraphAndRef -> GraphAndRef
deleteBindingsWithRef (GraphAndRef g r) = GraphAndRef (deleteBindings g) r

makeEdgesKeepBindings :: (Connection -> Edge) -> SyntaxGraph -> SyntaxGraph
makeEdgesKeepBindings = makeEdges'
-- | context from upper level
makeEdges :: (Connection -> Edge) -> SyntaxGraph -> SyntaxGraph
makeEdges e g = deleteBindings $ makeEdges' e g 


makeEdges' :: (Connection -> Edge) -> SyntaxGraph -> SyntaxGraph
makeEdges' edgeConstructor (SyntaxGraph icons edges sinks bindings eMap) = newGraph where
  (newSinks, newEdges) = makeEdgesCore edgeConstructor sinks bindings
  newGraph = SyntaxGraph icons (newEdges <> edges) newSinks bindings eMap

makeEdgesCore :: (Connection -> Edge) -> (Set.Set SgSink) -> (SMap.StringMap Reference) -> ((Set.Set SgSink), (Set.Set Edge))
makeEdgesCore edgeConstructor sinks bindings = (Set.fromList newSinks,Set.fromList newEdge) where
  -- TODO check if set->list->set gives optimal performance
  (newSinks, newEdge) = partitionEithers $ fmap renameOrMakeEdge (Set.toList sinks)
  renameOrMakeEdge :: SgSink -> Either SgSink Edge
  renameOrMakeEdge orig@(SgSink s destPort)
    = case SMap.lookup s bindings of
        Just ref -> case lookupReference bindings ref of
          Right sourcePort -> Right $ edgeConstructor (sourcePort, destPort)
          Left newStr -> Left $ SgSink newStr destPort
        Nothing -> Left orig
    