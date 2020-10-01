{-# LANGUAGE PatternSynonyms #-}

module SyntaxToGraphTests(
  syntaxToGraphTests
  ) where

import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.StringMap as SMap
import qualified Data.IntMap as IMap

import qualified Language.Haskell.Exts as Exts
import Control.Monad.State(State, evalState)

import SimpSyntaxToSyntaxGraph
  ( translateStringToSyntaxGraph
  , makeBox
  , evalExp
  )

import VisualTests(prettyShowSyntaxGraph)

import SyntaxGraph
  ( SyntaxGraph(..)
  , initialIdState
  , GraphAndRef(..)
  )
  
import  HsSyntaxToSimpSyntax (
    SimpExp(..)
    , SrcRef(..)
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
  )
  
syntaxToGraphTests :: Test
syntaxToGraphTests = TestList[
  TestLabel "SimpSyntaxToSyntaxGraph nodes" graphContainsNodesTests
  , TestLabel "SimpSyntaxToSyntaxGraph edges" syntaxGraphTests
  ]
-- SyntaxGraph
fileName :: String
fileName = "./examples/simpleFunctions.hs"

l = fileName

dummySrcRef = Exts.SrcSpan fileName 8 0 8 0

syntaxGraphTest :: SyntaxGraph -> SyntaxGraph -> Test
syntaxGraphTest expectedSyntaxGraph generatedSyntaxGraph 
  = TestCase
  (assertEqual 
    (prettyShowSyntaxGraph generatedSyntaxGraph)
    expectedSyntaxGraph -- pass mempty for example graph
    generatedSyntaxGraph 
  )

syntaxGraphTests :: Test
syntaxGraphTests = TestList [
  makeBoxTest
  , evalExpNameTest
  , evalExpNameInContextTest
  ]

makeBoxTest :: Test
makeBoxTest = syntaxGraphTest expectedSyntaxGraph generatedSyntaxGraph where
  generatedSyntaxGraph = fst $ evalState ( makeBox ("testText", dummySrcRef)) initialIdState
  expectedSyntaxGraph = SyntaxGraph
    {
    sgNodes = Set.fromList [
      Named {naName = NodeName 0, naVal = Embedder {emEmbedded = Set.fromList [], emNode = SyntaxNode {syntaxNodeCore = LiteralNode "testText", srcRef = Exts.SrcSpan "./examples/simpleFunctions.hs" 8 0 8 0}}}], 
    sgEdges = Set.fromList [], 
    sgSinks = Set.fromList [], 
    sgBinds = SMap.empty, 
    sgEmbedMap = IMap.fromList []
    } 

evalExpNameTest :: Test
evalExpNameTest = syntaxGraphTest expectedSyntaxGraph generatedSyntaxGraph where
  generatedSyntaxGraph = graph $ evalState state initialIdState where
    state = evalExp Set.empty (SimpExp dummySrcRef (SeName "testText"))
  expectedSyntaxGraph = SyntaxGraph
    {
    sgNodes = Set.fromList [
      Named {naName = NodeName 0, naVal = Embedder {emEmbedded = Set.fromList [], emNode = SyntaxNode {syntaxNodeCore = LiteralNode "testText", srcRef = Exts.SrcSpan "./examples/simpleFunctions.hs" 8 0 8 0}}}], 
    sgEdges = Set.fromList [], 
    sgSinks = Set.fromList [], 
    sgBinds = SMap.empty, 
    sgEmbedMap = IMap.fromList []
    } 

-- Shouldn't it make something?
evalExpNameInContextTest :: Test
evalExpNameInContextTest = syntaxGraphTest expectedSyntaxGraph generatedSyntaxGraph where
  generatedSyntaxGraph = graph $ evalState state initialIdState where
    state = evalExp (Set.fromList [nameInContext]) (SimpExp dummySrcRef (SeName nameInContext))
  nameInContext = "testText"
  expectedSyntaxGraph = SyntaxGraph
    {
    sgNodes = Set.fromList [],
    sgEdges = Set.fromList [], 
    sgSinks = Set.fromList [], 
    sgBinds = SMap.empty, 
    sgEmbedMap = IMap.fromList []
    } 

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
  ]


functionAplication = assertAllHaveNodesNamed
  (Set.fromList ["y","f","x"])
  ["y = f x"
  , "y = f $ x"
  ]

dollarTests = assertAllHaveNodesNamed
  (Set.fromList ["y","f","x","g"])
  [ "y = f (g x)"
  , "y = f $ (g x)"
  , "y = f $ g  $ x"
  , "y = f (g  $ x)"
  ]

composeApplyTests = assertAllHaveNodesNamed
  (Set.fromList ["y","f1","f2","f3","x"])
  [ "y = f3 (f2 (f1 x))"
  , "y = f3 . f2 . f1 $ x"
  , "y = (f3 . f2 . f1) x"
  ]

fmapTests = assertAllHaveNodesNamed
  (Set.fromList ["y","fmap","f","x"])
  [ "y = fmap f x"
  , "y = f <$> x"
  ]

infixTests = assertAllHaveNodesNamed
  (Set.fromList ["y","1","2","+"])
  [ "y = (+) 1 2"
  , "y = ((+) 1) 2"
  , "y = 1 + 2"
  , "y = (1 +) 2"
  ]

letTests = assertAllHaveNodesNamed 
  (Set.fromList ["y","f","1"])
  [ "y = f 1"
  , "y = let x = 1 in f x"
  , "y = let {b = a; a = 1} in f b"
  ]
  
letAndAplicationTests = assertAllHaveNodesNamed 
  (Set.fromList ["y","x"])
  [ "y x = x"
  , "y x = let z = x in z"
  ]
