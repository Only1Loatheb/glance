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
  , edgesForRefPortList
  , combineExpressions
  , makeApplyGraph
  , makeMultiIfGraph
  , namesInPattern
  , lookupReference
  , deleteBindings
  , makeEdges
  , makeBox
  , makeOutputEdgesAndSinks
  , makeEdge
  , EvalContext
  , GraphAndRef(..)
  , Reference
  , SgSink(..)
  , SgBind
  , strToGraphRef
  , grNamePortToGrRef
  , makeNestedPatternGraph
  , makeAsBindGraph
  , makePatternResult
  , initialIdState
  , makeListCompGraph
) where

import Control.Monad.State(State, state)
import Diagrams.Prelude((<>))
import Control.Monad(replicateM)
import Control.Monad.State(State, evalState)
import           Data.Either( partitionEithers
                            , fromRight
                            )
import qualified Data.Graph.Inductive.PatriciaTree as FGR
import Data.List(unzip5, partition, intercalate)
import Data.Maybe(fromMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.StringMap as SMap
import qualified Data.IntMap as IMap

import qualified Language.Haskell.Exts as Exts
import qualified Language.Haskell.Exts.Pretty as PExts

import           PortConstants(
  inputPort
  , resultPort
  , argumentPorts
  , caseValuePorts
  , casePatternPorts
  , argPortsConst
  , multiIfValuePorts
  , multiIfBoolPorts
  , mixedPorts
  , resultPortsConst
  )

import           Types(
  AnnotatedGraph
  , Labeled(..)
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
  )
import Util(makeSimpleEdge, nameAndPort, justName)
import HsSyntaxToSimpSyntax( SimpPat(..),  SimpExp(..))
import StringSymbols(
  listCompositionPlaceholderStr
  , typeSignatureSeparatorStr
  , typeNameSeparatorStr
  , negativeLiteralStr
  , patternWildCardStr
  , unusedArgumentStr
  , defaultPatternNameStr
  , nTupleString
  , nTupleSectionString
  , nListString
  )

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

-- BEGIN Constructors and Destructors

syntaxGraphFromNodes :: Set.Set SgNamedNode -> SyntaxGraph
syntaxGraphFromNodes icons = SyntaxGraph icons Set.empty Set.empty SMap.empty mempty

syntaxGraphFromNodesEdges :: Set.Set SgNamedNode -> Set.Set Edge -> SyntaxGraph
syntaxGraphFromNodesEdges icons edges = SyntaxGraph icons edges Set.empty SMap.empty mempty

bindsToSyntaxGraph :: SMap.StringMap Reference -> SyntaxGraph
bindsToSyntaxGraph binds = SyntaxGraph Set.empty Set.empty Set.empty binds mempty

sinksToSyntaxGraph :: Set.Set SgSink -> SyntaxGraph
sinksToSyntaxGraph sinks = SyntaxGraph Set.empty Set.empty sinks SMap.empty mempty

edgesToSyntaxGraph :: Set.Set Edge -> SyntaxGraph
edgesToSyntaxGraph edges = SyntaxGraph Set.empty edges mempty SMap.empty mempty

graphAndRefToGraph :: GraphAndRef -> SyntaxGraph
graphAndRefToGraph (GraphAndRef g _) = g

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

makeApplyGraph ::
  Int
  -> LikeApplyFlavor
  -> Bool
  -> NodeName
  -> GraphAndRef
  -> [GraphAndRef]
  -> (SyntaxGraph, NameAndPort)
makeApplyGraph numArgs applyFlavor inPattern applyIconName funVal argVals
  = (newGraph <> combinedGraph
    , nameAndPort applyIconName (resultPort applyNode)
    )
  where
    applyNode = ApplyNode applyFlavor numArgs
    argumentNamePorts
      = map (nameAndPort applyIconName) (argumentPorts applyNode)
    functionPort = nameAndPort applyIconName (inputPort applyNode)
    combinedGraph = combineExpressions inPattern
                    $ zip (funVal:argVals) (functionPort:argumentNamePorts)
    icons = [Named applyIconName (mkEmbedder applyNode)]
    newGraph = syntaxGraphFromNodes $ Set.fromList icons

makeMultiIfGraph ::
  Int
  -> NodeName
  -> [GraphAndRef]
  -> [GraphAndRef]
  -> (SyntaxGraph, NameAndPort)
makeMultiIfGraph numPairs multiIfName bools exps
  = (newGraph, nameAndPort multiIfName (resultPort multiIfNode))
  where
    multiIfNode = CaseOrMultiIfNode MultiIfTag numPairs
    expsWithPorts = zip exps $ map (nameAndPort multiIfName) multiIfValuePorts
    boolsWithPorts = zip bools $ map (nameAndPort multiIfName) multiIfBoolPorts
    combindedGraph = combineExpressions False $ expsWithPorts <> boolsWithPorts
    icons = [Named multiIfName (mkEmbedder multiIfNode)]
    newGraph = (syntaxGraphFromNodes $ Set.fromList icons) <> combindedGraph

-- | Make a syntax graph that has the bindings for a list of "as pattern" (@)
-- names.
makeAsBindGraph :: Reference -> [Maybe String] -> SyntaxGraph
makeAsBindGraph ref asNames
  = bindsToSyntaxGraph (SMap.fromList (mapMaybe makeBind asNames))
  where
    makeBind mName = case mName of
      Nothing -> Nothing
      Just asName -> Just $ (asName, ref)

makeNestedPatternGraph ::
  NodeName
  -> String
  -> [(GraphAndRef, Maybe String)]
  -> (SyntaxGraph, NameAndPort)
makeNestedPatternGraph applyIconName funStr argVals = nestedApplyResult
  where
    dummyNode = PatternApplyNode defaultPatternNameStr []

    argsAndPorts
      = zip argVals $ map (nameAndPort applyIconName) $ argumentPorts dummyNode
    mappedArgs = fmap patternArgumentMapper argsAndPorts

    (unnestedArgsAndPort, nestedNamedNodesAndGraphs)
      = partitionEithers (fmap snd mappedArgs)

    (nestedArgs, _, nestedSinks, nestedBinds, nestedEMaps)
      = graphsToComponents $ fmap snd nestedNamedNodesAndGraphs

    argListMapper (str, arg) = case arg of
      Left _ -> Labeled Nothing str
      Right (namedNode, _) -> Labeled (Just namedNode) str

    argList = fmap argListMapper mappedArgs

    combinedGraph = combineExpressions True unnestedArgsAndPort

    pAppNode = PatternApplyNode funStr argList
    icons = Set.singleton (Named applyIconName (mkEmbedder pAppNode))

    asNameBinds = mapMaybe asNameBind argVals
    allBinds = SMap.union nestedBinds (SMap.fromList asNameBinds)

    newEMap = IMap.fromList
              ((\(Named (NodeName n) _) -> (n, applyIconName))  <$> (Set.toList nestedArgs))
              <> nestedEMaps

    newGraph = SyntaxGraph
               icons
               Set.empty
               nestedSinks
               allBinds
               newEMap
    nestedApplyResult = (newGraph <> combinedGraph
                        , nameAndPort applyIconName (resultPort pAppNode))

patternArgumentMapper ::
  ((GraphAndRef, Maybe String), t)
  -> (String, Either (GraphAndRef, t) (SgNamedNode, SyntaxGraph))
patternArgumentMapper (asGraphAndRef@(graphAndRef, _), port)
  = (patName, eitherVal)
  where
    graph = graphAndRefToGraph graphAndRef
    patName = patternName asGraphAndRef

    nodes = sgNodes graph
    eitherVal = if Set.size nodes == 1 && Set.size (sgEdges graph) == 0
      then Right ((Set.elemAt 0 nodes), graph)
      else Left (graphAndRef, port)

asNameBind :: (GraphAndRef, Maybe String) -> Maybe SgBind
asNameBind (GraphAndRef _ ref, mAsName) = case mAsName of
  Nothing -> Nothing
  Just asName -> Just ( asName, ref)

makePatternResult :: Functor f =>
  f (SyntaxGraph, NameAndPort) -> f (GraphAndRef, Maybe String)
makePatternResult
  = fmap (\(graph, namePort) -> (GraphAndRef graph (Right namePort), Nothing))

makeListCompGraph :: SyntaxNode -> NameAndPort
                       -> SyntaxGraph -> ([GraphAndRef], [GraphAndRef], [GraphAndRef]) -> SyntaxGraph
makeListCompGraph listCompNode listCompItemRef listCompItem graphsAndRefs = combinedGraph where
  (NameAndPort listCompName _port) = listCompItemRef

  ( qualsGraphsAndRefs , genGraphsAndRefs , declGraphsAndRefs) = graphsAndRefs
  allGraphsAndRefs = qualsGraphsAndRefs ++ genGraphsAndRefs ++ declGraphsAndRefs 

  expGraphs = fmap graphAndRefToGraph allGraphsAndRefs
  expsGraph = mconcat expGraphs

  (edges, binds) = makeListCompEdges listCompName allGraphsAndRefs

  outputGraph = listCompOutputGraph edges binds listCompItemRef

  combinedGraph = deleteBindings . makeEdges $  listCompItem <> expsGraph <> outputGraph

  listCompOutputGraph ::  [Edge] -> [(SMap.Key, Reference)] -> NameAndPort -> SyntaxGraph
  listCompOutputGraph  patternEdges' newBinds' returnPort = graph where
    patternEdges = Set.fromList patternEdges'
    newBinds = SMap.fromList newBinds'
    (newEdges, newSinks) = makeOutputEdgesAndSinks (Right returnPort) patternEdges returnPort
    listCompIconSet = Set.singleton (Named listCompName (mkEmbedder listCompNode))
    graph = SyntaxGraph listCompIconSet newEdges newSinks newBinds mempty

  makeListCompEdges :: NodeName -> [GraphAndRef] -> ([Edge], [SgBind])
  makeListCompEdges listCompName qualifiersGraph = (edges, binds) where
    listCompPorts = map (nameAndPort listCompName) $ resultPortsConst
    (edges, binds) = partitionEithers $ zipWith makeEdge qualifiersGraph listCompPorts

-- strToGraphRef is not in SyntaxNodeToIcon, since it is only used by evalQName.
strToGraphRef :: EvalContext -> String -> State IDState GraphAndRef
strToGraphRef c str = fmap mapper (makeBox str) where
  mapper gr = if Set.member str c
    then GraphAndRef mempty (Left str)
    else grNamePortToGrRef gr

-- HELPER FUNCTIONS
-- lambda
makeEdge :: GraphAndRef -> NameAndPort -> Either Edge SgBind
makeEdge (GraphAndRef _ ref) lamPort = case ref of
  Right patPort -> Left $ makeSimpleEdge (lamPort, patPort)
  Left str -> Right (str, Right lamPort)

-- TODO: Refactor with combineExpressions
edgesForRefPortList :: Bool -> [(Reference, NameAndPort)] -> SyntaxGraph
edgesForRefPortList inPattern portExpPairs
  = mconcat $ fmap makeGraph portExpPairs
  where
    edgeOpts = if inPattern then [EdgeInPattern] else []
    makeGraph (ref, port) = case ref of
      Left str -> if inPattern
        then bindsToSyntaxGraph $ SMap.singleton str (Right port)
        else sinksToSyntaxGraph $ Set.singleton (SgSink str port)
      Right resPort -> edgesToSyntaxGraph $ Set.singleton  (Edge edgeOpts connection)
        where
          connection = if inPattern
                          -- If in a pattern, then the port on the case icon is
                          -- the data source.
                       then (port, resPort)
                       else (resPort, port)

combineExpressions :: Bool -> [(GraphAndRef, NameAndPort)] -> SyntaxGraph
combineExpressions inPattern portExpPairs
  = mconcat $ fmap makeGraph portExpPairs
  where
    edgeOpts = if inPattern then [EdgeInPattern] else []
    makeGraph (GraphAndRef graph ref, port) = graph <> case ref of
      Left str -> if inPattern
        then bindsToSyntaxGraph $ SMap.singleton str (Right port)
        else sinksToSyntaxGraph $ Set.singleton  (SgSink str port)
      Right resPort -> edgesToSyntaxGraph $ Set.singleton (Edge edgeOpts (resPort, port))


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
lookupReference bindings ref@(Left originalS) = lookupReference' (Just ref) where

  lookupReference' :: Maybe Reference -> Reference
  lookupReference'  (Just newRef@(Right _)) = newRef
  lookupReference'  (Just (Left s))
    = failIfCycle originalS foundRef $ lookupReference' foundRef where
      foundRef =SMap.lookup s  bindings
  lookupReference'  _Nothing  = error "lookupReference filed"

  failIfCycle ::  String -> Maybe Reference -> Reference -> Reference
  failIfCycle originalS (Just r@(Left newStr)) res  = if newStr == originalS then r else res
  failIfCycle _ _ res = res

deleteBindings :: SyntaxGraph -> SyntaxGraph
deleteBindings (SyntaxGraph a b c _ e) = SyntaxGraph a b c SMap.empty e

makeEdgesCore :: (Set.Set SgSink) -> (SMap.StringMap Reference) -> ((Set.Set SgSink), (Set.Set Edge))
makeEdgesCore sinks bindings = (Set.fromList newSinks,Set.fromList newEdge) where
  -- TODO check if set->list->set gives optimal performance
  (newSinks, newEdge) = partitionEithers $ fmap renameOrMakeEdge (Set.toList sinks)
  renameOrMakeEdge :: SgSink -> Either SgSink Edge
  renameOrMakeEdge orig@(SgSink s destPort)
    = case SMap.lookup s bindings of
        Just ref -> case lookupReference bindings ref of
          Right sourcePort -> Right $ makeSimpleEdge (sourcePort, destPort)
          Left newStr -> Left $ SgSink newStr destPort
        Nothing -> Left orig

makeEdges :: SyntaxGraph -> SyntaxGraph
makeEdges (SyntaxGraph icons edges sinks bindings eMap) = newGraph where
  (newSinks, newEdges) = makeEdgesCore sinks bindings
  newGraph = SyntaxGraph icons (newEdges <> edges) newSinks bindings eMap

makeBox :: String -> State IDState (SyntaxGraph, NameAndPort)
makeBox str = do
  name <- getUniqueName
  let graph
        = syntaxGraphFromNodes (Set.singleton (Named name (mkEmbedder (LiteralNode str))))
  pure (graph, justName name)

patternName :: (GraphAndRef, Maybe String) -> String
patternName (GraphAndRef _ ref, mStr) = fromMaybe
  (case ref of
    Left str -> str
    Right _ -> defaultPatternNameStr
  )
  mStr

makeOutputEdgesAndSinks :: Either String NameAndPort
  -> Set.Set Edge -> NameAndPort -> (Set.Set Edge, Set.Set SgSink)
makeOutputEdgesAndSinks rhsRef patternEdges returnPort = case rhsRef of
  Left s -> (patternEdges, Set.singleton (SgSink s returnPort))
  Right rhsPort -> (Set.insert (makeSimpleEdge (rhsPort, returnPort)) patternEdges, mempty)