module SyntaxToGraphTests(
  syntaxToGraphTests
  ) where

import Test.HUnit
import qualified Data.Set as Set

import SimpSyntaxToSyntaxGraph(translateStringToSyntaxGraph)

import SyntaxGraph(SyntaxGraph(..))
import Types( 
  Embedder(..)
  , SyntaxNode(..)
  , SyntaxNodeCore(..)
  , Labeled(..)
  , Named(..)
  )

assertAllWith :: (String -> Set.Set String) -> Set.Set String -> [String] -> Test
assertAllWith f identifiers codeStrings = TestList $ fmap (identifiers ~=?) (map f codeStrings)

assertAllHaveNodesNamed :: Set.Set String -> [String] -> Test
assertAllHaveNodesNamed = assertAllWith getNodeNames

getNodeNames :: String -> Set.Set String
getNodeNames codeString 
  = mconcat $ map (Set.fromList . syntaxNodeCoreToMaybeStr . syntaxNodeCore . emNode . naVal) 
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

syntaxToGraphTests :: Test
syntaxToGraphTests = TestList[
  TestLabel "SyntaxToGraphTests.hs" graphContainsNodesTests
  ]

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