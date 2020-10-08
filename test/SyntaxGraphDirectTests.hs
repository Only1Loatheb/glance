{-# LANGUAGE PatternSynonyms #-}

module SyntaxGraphDirectTests(
  syntaxGraphDirectTests
  ) where

import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.StringMap as SMap
import qualified Data.IntMap as IMap

import qualified Language.Haskell.Exts as Exts
import Control.Monad.State(evalState)

import SimpSyntaxToSyntaxGraph
  ( translateStringToSyntaxGraph
  , makeBox
  , evalExp
  , evalDecl
  )

import VisualTests(prettyShowSyntaxGraph)

import SyntaxGraph
  ( SyntaxGraph(..)
  , initialIdState
  , GraphAndRef(..)
  )
  
import  HsSyntaxToSimpSyntax (
    SimpExp(..)
    , SrcRef
    , SelectorAndVal(..)
    , SimpAlt(..)
    , SimpDecl(..)
    , SimpPat(..)
    , SimpQStmt(..)
    , SimpQStmtCore(..)
    , SimpExpCore(..)
    , SimpPatCore(..)
    , SimpDeclCore(..)
    , stringToSimpDecl
    , qNameToString
    , nameToString
    , customParseDecl
    , hsDeclToSimpDecl
    , formatString
    , pattern FunctionCompositionStr
    , simpPatNameStr
    )

import Types( 
  Embedder(..)
  , SyntaxNodeCore(..)
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
  , Connection(..)
  , Port(..)
  )
  
syntaxGraphDirectTests :: Test
syntaxGraphDirectTests = TestList[
  TestLabel "SimpSyntaxToSyntaxGraph nodes" graphContainsNodesTests
  , TestLabel "SimpSyntaxToSyntaxGraph edges" syntaxGraphComparisonTests
  ]
-- SyntaxGraph
fileName :: String
fileName = "./f.hs"

dummySrcRef :: Exts.SrcSpan
dummySrcRef = Exts.SrcSpan fileName 8 0 8 0

dummySrcInfo :: Exts.SrcSpanInfo
dummySrcInfo = Exts.SrcSpanInfo dummySrcRef []

syntaxGraphTest :: SyntaxGraph -> SyntaxGraph -> Test
syntaxGraphTest expectedSyntaxGraph generatedSyntaxGraph 
  = TestCase
  (assertEqual 
    " "--(prettyShowSyntaxGraph generatedSyntaxGraph)
    expectedSyntaxGraph -- pass mempty for example graph
    generatedSyntaxGraph 
  )

syntaxGraphComparisonTests :: Test
syntaxGraphComparisonTests = TestList [
  makeBoxTest
  , evalExpNameTest
  , evalExpNameInContextTest
  , evalExpLitTest
  , evalExpAppTest
  , translateStrFunDef
  , evalDeclFunDefTest
  ]

makeBoxTest :: Test
makeBoxTest = syntaxGraphTest expectedSyntaxGraph generatedSyntaxGraph where
  generatedSyntaxGraph = fst $ evalState ( makeBox ("testText", dummySrcRef)) initialIdState
  expectedSyntaxGraph = SyntaxGraph {
    sgNodes = Set.fromList [
      Named {naName = NodeName 0, naVal = Embedder {emEmbedded = Set.fromList [], emNode = SyntaxNode {syntaxNodeCore = LiteralNode "testText", srcRef = dummySrcRef}}}], 
    sgEdges = Set.fromList [], 
    sgSinks = Set.fromList [], 
    sgBinds = SMap.empty, 
    sgEmbedMap = IMap.fromList []
    } 

evalExpNameTest :: Test
evalExpNameTest = syntaxGraphTest expectedSyntaxGraph generatedSyntaxGraph where
  generatedSyntaxGraph = graph $ evalState state initialIdState where
    state = evalExp Set.empty (SimpExp dummySrcRef (SeName "testText"))
  expectedSyntaxGraph = SyntaxGraph {
    sgNodes = Set.fromList [
      Named {naName = NodeName 0, naVal = Embedder {emEmbedded = Set.fromList [], emNode = SyntaxNode {syntaxNodeCore = LiteralNode "testText", srcRef = dummySrcRef}}}], 
    sgEdges = Set.fromList [], 
    sgSinks = Set.fromList [], 
    sgBinds = SMap.empty, 
    sgEmbedMap = IMap.fromList []
    } 

-- Shouldn't it create something?
evalExpNameInContextTest :: Test
evalExpNameInContextTest = syntaxGraphTest expectedSyntaxGraph generatedSyntaxGraph where
  generatedSyntaxGraph = graph $ evalState state initialIdState where
    state = evalExp (Set.fromList [nameInContext]) (SimpExp dummySrcRef (SeName nameInContext))
  nameInContext = "testText"
  expectedSyntaxGraph = SyntaxGraph{
    sgNodes = Set.fromList [],
    sgEdges = Set.fromList [], 
    sgSinks = Set.fromList [], 
    sgBinds = SMap.empty, 
    sgEmbedMap = IMap.fromList []
    }

evalExpLitTest :: Test
evalExpLitTest = syntaxGraphTest expectedSyntaxGraph generatedSyntaxGraph where
  generatedSyntaxGraph = graph $ evalState state initialIdState where
    state = evalExp Set.empty (SimpExp dummySrcRef (SeLit lit))
  lit :: Exts.Literal Exts.SrcSpanInfo
  lit = Exts.String dummySrcInfo "test" "test" 
  expectedSyntaxGraph = SyntaxGraph {
    sgNodes = Set.fromList [
      Named {naName = NodeName 0, naVal = Embedder {emEmbedded = Set.fromList [], emNode = SyntaxNode {syntaxNodeCore = LiteralNode "\"test\"", srcRef = dummySrcRef}}}], 
    sgEdges = Set.fromList [], 
    sgSinks = Set.fromList [], 
    sgBinds = SMap.empty, 
    sgEmbedMap = IMap.fromList []
    } 

evalExpAppTest :: Test
evalExpAppTest = syntaxGraphTest expectedSyntaxGraph generatedSyntaxGraph where
  generatedSyntaxGraph = graph $ evalState state initialIdState where
    state = evalExp Set.empty (SimpExp dummySrcRef (SeApp fun arg))
  fun = SimpExp dummySrcRef (SeName "y")
  arg = SimpExp dummySrcRef (SeName "x")
  expectedSyntaxGraph = SyntaxGraph {
    sgNodes = Set.fromList [
      Named {naName = NodeName 0, naVal = Embedder {emEmbedded = Set.fromList [], emNode = SyntaxNode {syntaxNodeCore = LiteralNode "y", srcRef = dummySrcRef}}}
      , Named {naName = NodeName 1, naVal = Embedder {emEmbedded = Set.fromList [], emNode = SyntaxNode {syntaxNodeCore = LiteralNode "x", srcRef = dummySrcRef }}}
      , Named {naName = NodeName 2, naVal = Embedder {emEmbedded = Set.fromList [], emNode = SyntaxNode {syntaxNodeCore = ApplyNode ApplyNodeFlavor 1, srcRef = dummySrcRef}}}]
    , sgEdges = Set.fromList [
      Edge {edgeOption = DrawAndConstraint, edgeConnection = (NameAndPort (NodeName 0) (Port 1),NameAndPort (NodeName 2) (Port 0))}
      , Edge {edgeOption = DrawAndConstraint, edgeConnection = (NameAndPort (NodeName 1) (Port 1),NameAndPort (NodeName 2) (Port 2))}]
    , sgSinks = Set.fromList []
    , sgBinds = SMap.empty
    , sgEmbedMap = IMap.fromList []
    }

translateStrFunDef :: Test
translateStrFunDef = syntaxGraphTest expectedSyntaxGraph generatedSyntaxGraph where
  generatedSyntaxGraph = translateStringToSyntaxGraph "f x = x"
  expectedSyntaxGraph = SyntaxGraph {
    sgNodes = Set.fromList [
      Named {naName = NodeName 0, naVal = Embedder {emEmbedded = Set.fromList [], emNode = SyntaxNode {syntaxNodeCore = FunctionValueNode "f" (Set.fromList []), srcRef = Exts.SrcSpan "<unknown>.hs" 1 1 1 8}}}
      ,Named {naName = NodeName 1, naVal = Embedder {emEmbedded = Set.fromList [], emNode = SyntaxNode {syntaxNodeCore = FunctionArgNode ["x"], srcRef = Exts.SrcSpan "<unknown>.hs" 1 1 1 8}}}
      ,Named {naName = NodeName 3, naVal = Embedder {emEmbedded = Set.fromList [], emNode = SyntaxNode {syntaxNodeCore = BindNameNode "f", srcRef = Exts.SrcSpan "<unknown>.hs" 1 1 1 8}}}]
    , sgEdges = Set.fromList [
      Edge {edgeOption = DrawAndConstraint, edgeConnection = (NameAndPort (NodeName 0) (Port 1),NameAndPort (NodeName 3) (Port 0))}
      ,Edge {edgeOption = DrawAndConstraint, edgeConnection = (NameAndPort (NodeName 1) (Port 3),NameAndPort (NodeName 0) (Port 0))}
      ,Edge {edgeOption = DoNotDrawButConstraint, edgeConnection = (NameAndPort (NodeName 1) (Port 1),NameAndPort (NodeName 0) (Port 0))}]
    , sgSinks = Set.fromList []
    , sgBinds = SMap.empty
    , sgEmbedMap = IMap.fromList []
    }

-- f x = 1
evalDeclFunDefTest :: Test
evalDeclFunDefTest = syntaxGraphTest expectedSyntaxGraph generatedSyntaxGraph where
  generatedSyntaxGraph = translateStringToSyntaxGraph "f x = 1"
  expectedSyntaxGraph = SyntaxGraph {
    sgNodes = Set.fromList [
      Named {naName = NodeName 0, naVal = Embedder {emEmbedded = Set.fromList [], emNode = SyntaxNode {syntaxNodeCore = FunctionValueNode "f" (Set.fromList [NodeName 2]), srcRef = Exts.SrcSpan "<unknown>.hs" 1 1 1 8}}}
      , Named {naName = NodeName 1, naVal = Embedder {emEmbedded = Set.fromList [], emNode = SyntaxNode {syntaxNodeCore = FunctionArgNode ["x"], srcRef = Exts.SrcSpan "<unknown>.hs" 1 1 1 8}}}
      , Named {naName = NodeName 2, naVal = Embedder {emEmbedded = Set.fromList [], emNode = SyntaxNode {syntaxNodeCore = LiteralNode "1", srcRef = Exts.SrcSpan "<unknown>.hs" 1 7 1 8}}}
      , Named {naName = NodeName 3, naVal = Embedder {emEmbedded = Set.fromList [], emNode = SyntaxNode {syntaxNodeCore = BindNameNode "f", srcRef = Exts.SrcSpan "<unknown>.hs" 1 1 1 8}}}]
    , sgEdges = Set.fromList [
      Edge {edgeOption = DrawAndConstraint, edgeConnection = (NameAndPort (NodeName 0) (Port 1),NameAndPort (NodeName 3) (Port 0))}
      ,Edge {edgeOption = DrawAndConstraint, edgeConnection = (NameAndPort (NodeName 2) (Port 1),NameAndPort (NodeName 0) (Port 0))}
      ,Edge {edgeOption = DoNotDrawButConstraint, edgeConnection = (NameAndPort (NodeName 1) (Port 1),NameAndPort (NodeName 0) (Port 0))}]
    , sgSinks = Set.fromList []
    , sgBinds = SMap.empty
    , sgEmbedMap = IMap.fromList []
    }

-- FGL Graph:
-- 0:Named {naName = NodeName 0, naVal = Embedder {emEmbedded = fromList [], emNode = SyntaxNode {syntaxNodeCore = FunctionValueNode "f" (fromList []), srcRef = SrcSpan "<unknown>.hs" 1 1 1 8}}}->[(Edge {edgeOption = DrawAndConstraint, edgeConnection = (NameAndPort (NodeName 0) (Port 1),NameAndPort (NodeName 3) (Port 0))},3)]
-- 1:Named {naName = NodeName 1, naVal = Embedder {emEmbedded = fromList [], emNode = SyntaxNode {syntaxNodeCore = FunctionArgNode ["x"], srcRef = SrcSpan "<unknown>.hs" 1 1 1 8}}}->[(Edge {edgeOption = DoNotDrawButConstraint, edgeConnection = (NameAndPort (NodeName 1) (Port 1),NameAndPort (NodeName 0) (Port 0))},0),(Edge {edgeOption = DrawAndConstraint, edgeConnection = (NameAndPort (NodeName 1) (Port 3),NameAndPort (NodeName 0) (Port 0))},0)]
-- 3:Named {naName = NodeName 3, naVal = Embedder {emEmbedded = fromList [], emNode = SyntaxNode {syntaxNodeCore = BindNameNode "f", srcRef = SrcSpan "<unknown>.hs" 1 1 1 8}}}->[]
-- Collapsed Graph:
-- 0:NodeInfo {niParent = Nothing, niVal = Named {naName = NodeName 0, naVal = Embedder {emEmbedded = fromList [], emNode = SyntaxNode {syntaxNodeCore = FunctionValueNode "f" (fromList []), srcRef = SrcSpan "<unknown>.hs" 1 1 1 8}}}}->[(EmbedInfo {eiEmbedDir = Nothing, eiVal = Edge {edgeOption = DrawAndConstraint, edgeConnection = (NameAndPort (NodeName 0) (Port 1),NameAndPort (NodeName 3) (Port 0))}},3)]
-- 1:NodeInfo {niParent = Nothing, niVal = Named {naName = NodeName 1, naVal = Embedder {emEmbedded = fromList [], emNode = SyntaxNode {syntaxNodeCore = FunctionArgNode ["x"], srcRef = SrcSpan "<unknown>.hs" 1 1 1 8}}}}->[(EmbedInfo {eiEmbedDir = Nothing, eiVal = Edge {edgeOption = DrawAndConstraint, edgeConnection = (NameAndPort (NodeName 1) (Port 3),NameAndPort (NodeName 0) (Port 0))}},0),(EmbedInfo {eiEmbedDir = Nothing, eiVal = Edge {edgeOption = DoNotDrawButConstraint, edgeConnection = (NameAndPort (NodeName 1) (Port 1),NameAndPort (NodeName 0) (Port 0))}},0)]
-- 3:NodeInfo {niParent = Nothing, niVal = Named {naName = NodeName 3, naVal = Embedder {emEmbedded = fromList [], emNode = SyntaxNode {syntaxNodeCore = BindNameNode "f", srcRef = SrcSpan "<unknown>.hs" 1 1 1 8}}}}->[]
-- end SyntaxGraph
-- Nodes
assertAllWith :: (String -> Set.Set String) -> Set.Set String -> [String] -> Test
assertAllWith f identifiers codeStrings = TestList $ fmap (identifiers ~=?) (map f codeStrings)
  
assertAllHaveNodesNamed :: Set.Set String -> [String] -> Test
assertAllHaveNodesNamed = assertAllWith getNodeNames

syntaxNodeToIdentifierSet :: Embedder SyntaxNode -> Set.Set String
syntaxNodeToIdentifierSet = Set.fromList . syntaxNodeCoreToMaybeStr . syntaxNodeCore . emNode

getNodeNames :: String -> Set.Set String
getNodeNames codeString 
  = mconcat $ map (syntaxNodeToIdentifierSet . naVal) 
  $ Set.toList . sgNodes $ translateStringToSyntaxGraph codeString

syntaxNodeCoreToMaybeStr  :: SyntaxNodeCore -> [String]
syntaxNodeCoreToMaybeStr (ApplyNode {}) = []
syntaxNodeCoreToMaybeStr (PatternApplyNode name labeledList) = name : map laLabel labeledList
syntaxNodeCoreToMaybeStr (NameNode name) = [name]
syntaxNodeCoreToMaybeStr (BindNameNode name) = [name]
syntaxNodeCoreToMaybeStr (LiteralNode name) = [name]
syntaxNodeCoreToMaybeStr (FunctionArgNode names) = names
syntaxNodeCoreToMaybeStr (FunctionValueNode name _) = [name]
syntaxNodeCoreToMaybeStr (CaseResultNode {}) = []
syntaxNodeCoreToMaybeStr (CaseOrMultiIfNode {}) = []
syntaxNodeCoreToMaybeStr (ListCompNode {}) = []
-- end Nodes
graphContainsNodesTests :: Test
graphContainsNodesTests = TestList [
  functionAplication
  , dollarTests
  , composeApplyTests
  , fmapTests
  , infixTests
  , letTests
  , letAndAplicationTests
  ]

functionAplication :: Test
functionAplication = assertAllHaveNodesNamed
  (Set.fromList ["y","f","x"])
  ["y = f x"
  , "y = f $ x"
  ]
dollarTests :: Test
dollarTests = assertAllHaveNodesNamed
  (Set.fromList ["y","f","x","g"])
  [ "y = f (g x)"
  , "y = f $ (g x)"
  , "y = f $ g  $ x"
  , "y = f (g  $ x)"
  ]
composeApplyTests :: Test
composeApplyTests = assertAllHaveNodesNamed
  (Set.fromList ["y","f1","f2","f3","x"])
  [ "y = f3 (f2 (f1 x))"
  , "y = f3 . f2 . f1 $ x"
  , "y = (f3 . f2 . f1) x"
  ]
fmapTests :: Test
fmapTests = assertAllHaveNodesNamed
  (Set.fromList ["y","fmap","f","x"])
  [ "y = fmap f x"
  , "y = f <$> x"
  ]
infixTests :: Test
infixTests = assertAllHaveNodesNamed
  (Set.fromList ["y","1","2","+"])
  [ "y = (+) 1 2"
  , "y = ((+) 1) 2"
  , "y = 1 + 2"
  , "y = (1 +) 2"
  ]
letTests :: Test
letTests = assertAllHaveNodesNamed 
  (Set.fromList ["y","f","1"])
  [ "y = f 1"
  , "y = let x = 1 in f x"
  , "y = let {b = a; a = 1} in f b"
  ]
letAndAplicationTests :: Test  
letAndAplicationTests = assertAllHaveNodesNamed 
  (Set.fromList ["y","x"])
  [ "y x = x"
  , "y x = let z = x in z"
  ]
