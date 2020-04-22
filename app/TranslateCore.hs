module TranslateCore(
  Reference,
  SyntaxGraph(..),
  EvalContext,
  GraphAndRef(..),
  SgSink(..),
  SgBind,
  syntaxGraphFromNodes,
  syntaxGraphFromNodesEdges,
  bindsToSyntaxGraph,
  graphAndRefToGraph,
  getUniqueName,
  getUniqueString,
  edgesForRefPortList,
  combineExpressions,
  makeApplyGraph,
  makeMultiIfGraph,
  namesInPattern,
  lookupReference,
  deleteBindings,
  makeEdges,
  makeBox,
  nTupleString,
  nTupleSectionString,
  nListString,
  syntaxGraphToFglGraph,
  nodeToIcon,
  initialIdState
) where

import Control.Monad.State(State, state)
import Data.Either(partitionEithers)
import qualified Data.Graph.Inductive.Graph as ING
import qualified Data.Graph.Inductive.PatriciaTree as FGR
import Data.List(find)
import qualified Data.StringMap as SMap
import qualified Data.IntMap as IMap
import Data.Semigroup(Semigroup, (<>))
import qualified Data.Set as Set

import PortConstants(inputPort, resultPort, argumentPorts, multiIfValuePorts
            , multiIfBoolPorts)
import Types(Labeled(..), Icon(..), SyntaxNode(..), Edge(..), EdgeOption(..)
            , NameAndPort(..), IDState, SgNamedNode, NodeName(..), Port
            , LikeApplyFlavor(..), CaseOrMultiIfTag(..), IDState(..)
            , Embedder(..), mkEmbedder, Named(..)
            , EmbedderSyntaxNode)
import Util(nameAndPort, makeSimpleEdge, justName, maybeBoolToBool
           , nodeNameToInt)
import StringSymbols(
  nTupleString
  , nTupleSectionString
  , nListString
  )

{-# ANN module "HLint: ignore Use list comprehension" #-}

-- OVERVIEW --
-- This module has the core functions and data types used by Translate.
-- This module also contains most/all of the translation functions that
-- do not require Language.Haskell.Exts.

type Reference = Either String NameAndPort

type EvalContext = [String]

type SgBind = (String, Reference)

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

namesInPattern :: (GraphAndRef, Maybe String) -> [String]
namesInPattern (graphAndRef, mName) = case mName of
  Nothing -> otherNames
  Just n -> n : otherNames
  where
    otherNames = namesInPatternHelper graphAndRef

    namesInPatternHelper :: GraphAndRef -> [String]
    namesInPatternHelper (GraphAndRef graph ref) = case ref of
      Left str -> [str]
      Right _ -> SMap.keys (sgBinds graph)

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

nodeToIcon :: EmbedderSyntaxNode -> Icon
nodeToIcon (Embedder embeddedNodes node) = case node of
  (ApplyNode flavor x)
    -> nestedApplySyntaxNodeToIcon flavor x embeddedNodes
  (PatternApplyNode s children)
    -> nestedPatternNodeToIcon s children
  (NameNode s) -> TextBoxIcon s
  (BindNameNode s) -> BindTextBoxIcon s
  (LiteralNode s) -> TextBoxIcon s
  (FunctionDefNode labels str bodyNodes)
    -> nestedLambdaToIcon labels embeddedNodes str bodyNodes
  CaseResultNode -> CaseResultIcon
  (CaseOrMultiIfNode tag x)
    -> nestedCaseOrMultiIfNodeToIcon tag x embeddedNodes
  (ListCompNode) -> listCompIcon embeddedNodes -- TODO actualy embede nodes

listCompIcon embeddedNodes =  
  ListCompIcon argList
  where
    dummyNode = ListCompNode
    argPorts = take (Set.size embeddedNodes) (argumentPorts dummyNode)
    argList = fmap (makeArg embeddedNodes) argPorts

-- | Helper for makeArg
findArg :: Port -> (NodeName, Edge) -> Bool
findArg currentPort
  (argName
  , Edge _ (NameAndPort fromName fromPort, NameAndPort toName toPort))
  | argName == fromName = maybeBoolToBool $ fmap (== currentPort) toPort
  | argName == toName = maybeBoolToBool $ fmap (== currentPort) fromPort
  | otherwise = False -- This case should never happen

makeArg :: Set.Set (NodeName, Edge) -> Port -> Maybe NodeName
makeArg args port = fst <$> find (findArg port) args

nestedApplySyntaxNodeToIcon :: LikeApplyFlavor
                            -> Int
                            -> Set.Set (NodeName, Edge)
                            -> Icon
nestedApplySyntaxNodeToIcon flavor numArgs args =
  NestedApply flavor headIcon argList
  where
    dummyNode = ApplyNode flavor numArgs
    argPorts = take numArgs (argumentPorts dummyNode)
    headIcon = makeArg args (inputPort dummyNode)
    argList = fmap (makeArg args) argPorts

nestedLambdaToIcon ::
  [String]  -- labels
  -> Set.Set (NodeName, Edge)  -- embedded icons
  -> String
  -> Set.Set NodeName  -- body nodes
  -> Icon
nestedLambdaToIcon labels embeddedNodes str =
  LambdaIcon labels (Labeled embeddedBodyNode str)
  where
    dummyNode = FunctionDefNode [] str Set.empty
    embeddedBodyNode = makeArg embeddedNodes (inputPort dummyNode)

nestedCaseOrMultiIfNodeToIcon ::
  CaseOrMultiIfTag
  -> Int
  -> Set.Set (NodeName, Edge)
  -> Icon
nestedCaseOrMultiIfNodeToIcon tag numArgs args = case tag of
  CaseTag -> NestedCaseIcon argList
  MultiIfTag -> NestedMultiIfIcon argList
  where
    dummyNode = CaseOrMultiIfNode CaseTag numArgs
    argPorts = take (2 * numArgs) $ argumentPorts dummyNode
    argList = fmap (makeArg args) (inputPort dummyNode : argPorts)

nestedPatternNodeToIcon :: String -> [Labeled (Maybe SgNamedNode)] -> Icon
nestedPatternNodeToIcon str children = NestedPatternApp
  (pure (Just (Named (NodeName (-1)) (TextBoxIcon str))))
  -- Why so many fmaps?
  ( (fmap . fmap . fmap . fmap) nodeToIcon children)

makeLNode :: SgNamedNode -> ING.LNode SgNamedNode
makeLNode namedNode@(Named (NodeName name) _) = (name, namedNode)

lookupInEmbeddingMap :: NodeName -> IMap.IntMap NodeName -> NodeName
lookupInEmbeddingMap origName eMap = lookupHelper origName where
  lookupHelper :: NodeName -> NodeName
  lookupHelper name@(NodeName nameInt) = case IMap.lookup nameInt eMap of
    Nothing -> name
    Just parent -> if parent == origName
      then error $ "lookupInEmbeddingMap: Found cycle. Node = "
           ++ show origName ++ "\nEmbedding Map = " ++ show eMap
      else lookupHelper parent

syntaxGraphToFglGraph :: SyntaxGraph -> FGR.Gr SgNamedNode Edge
syntaxGraphToFglGraph (SyntaxGraph nodes edges _ _ eMap) =
  ING.mkGraph (fmap makeLNode (Set.toList nodes)) labeledEdges where
    labeledEdges = fmap makeLabeledEdge (Set.toList edges)

    makeLabeledEdge e@(Edge _ (NameAndPort name1 _, NameAndPort name2 _)) =
      (nodeNameToInt $ lookupInEmbeddingMap name1 eMap
      , nodeNameToInt $ lookupInEmbeddingMap name2 eMap
      , e)
