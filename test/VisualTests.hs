{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module VisualTests(
  visualTranslateTests
  , prettyShowSyntaxGraph
  ) where
import qualified Language.Haskell.Exts as Exts
import Diagrams.Prelude hiding ((#), (&))
import qualified Data.Graph.Inductive.Graph as ING
import Data.List(intercalate)
import GHC.Stack(HasCallStack)
import qualified Data.Set as Set
import qualified Data.Map as SMap

import HsSyntaxToSimpSyntax(stringToSimpDecl, hsDeclToSimpDecl)
import SyntaxGraph( SyntaxGraph(..))
import SimpSyntaxToSyntaxGraph(translateStringToSyntaxGraph, customParseDecl, translateDeclToSyntaxGraph)
import CollapseGraph(syntaxGraphToCollapsedGraph, syntaxGraphToFglGraph)
import Rendering(renderIngSyntaxGraph)
import DrawingColors (dummyColorStyle)


import Types(
  Drawing
  , DrawingBackend
  , AnnotatedFGR
  )

import TextBox(multilineComment)

translateDeclToCollapsedGraph :: Exts.Decl Exts.SrcSpanInfo -> AnnotatedFGR
translateDeclToCollapsedGraph
  = syntaxGraphToCollapsedGraph . translateDeclToSyntaxGraph . hsDeclToSimpDecl

prettyShowList :: Show a => [a] -> String
prettyShowList ls = intercalate "\n" $ fmap show ls

prettyShowSyntaxGraph :: SyntaxGraph -> String
prettyShowSyntaxGraph (SyntaxGraph nodes edges sinks sources _) =
  "SyntaxGraph nodes:\n" ++
  prettyShowList (Set.toList nodes) ++
  "\nSyntaxGraph edges:\n" ++
  prettyShowList (Set.toList edges) ++
  "\nSyntaxGRaph sinks:\n" ++
  prettyShowList (Set.toList sinks) ++
  "\nSyntaxGraph sources:\n" ++
  prettyShowList (SMap.toList sources)

simpleTests :: [String]
simpleTests = [
  "y = x"
  , "y = 1"
  , "f = \\x -> x"
  -- , "f x = x"
  , "f x = 1"
  , "y = \"foo\""
  , "y = f x"
  , "y = f . g"
  , "y = f . g . h"
  , "y = f . g . h . i"
  , "y = (f . g) x"
  , "y = f (g x)"
  , "y = f $ g x"
  ]
enumTest :: [String]
enumTest = [
  "enumFrom = [1..]"
  ,"enumFromTo = [1..5]"
  ,"enumFromThen = [1,2..]"
  ,"enumFromThenTo = [1,2..10]"
  ]
typeSigTests :: [String]
typeSigTests = [
  "f :: a"
  , "f :: Int -> Bool"
  ]

dataDeclTests :: [String]
dataDeclTests = [
  "data Foo"
  , "data Foo = Foo Int"
  ]

composeTests :: [String]
composeTests = [
  "y = f (g x)",
  "y = f . g",
  "y = f3 . f2 . f1",
  "y = f3 . f2 . f1 $ x",
  -- TODO The nesting depth could be reduced if the composition is applied to
  -- both f3 and f4 instead of just f3.
  "y = f1 $ f6 (f2 (f3 . f4)) (f5 x)",
  "y = f x where f = g . h"
  ]

-- | nestedTests / collapseTest
nestedTests :: [String]
nestedTests = [
  "y = (\\x -> x) 0",
  "y = f (\\x -> x)",
  "y = f x",
  "y = let x = 1 in f (g x)",
  "y = f []",
  "y = f [1]",
  "y = f [1,2]",
  "y = f [g 3, h 5]",
  "y = f $ g (\\x -> x)",
  "y = f y",
  "y = f (g y)",
  "y = f1 (f2 ( f3 (f4 2))) 1", -- test compose embedded in apply
  "y = f0 $ f1 $ f2 z (f4 $ f5 q) $ x", -- compose embedded in compose
  "fibs = cons 1 (zipWith (+) fibs (tail fibs))",
  "y = foo (3 + bazOf2) bazOf2 where bazOf2 = baz 2",
  "y = foo (3 + bazOf2) (8 * bazOf2) where bazOf2 = baz 2",
  "Foo x = 1",
  "Foo 1 x = 2",
  "Foo (Bar x) = 1",
  "Foo (Bar (Baz x)) = 1",
  "Foo (Bar (Baz (Foot x))) = 1",
  "Foo (Bar x) (Baz y) = 1",
  "Foo (Bar x) = f 2",
  "Foo (Bar x) = f x",
  "y x = case x of {Just w -> (let (z,_) = w in z)}",
  "y = case x of 1 -> f 0",
  "y (Port x) = case x of 0 -> 1",
  "y (x@(Foo y)) = if 0 then x else 1"
  ]

specialTests :: [String]
specialTests = [
  "lookupTail EndAp1Arg = (arrowTail .~ dart')",
  "y = x .~ y",
  "initialIdState = IDState 0",
  "y = f x",
  "yyy = fff xxx",
  "yyyyy = fffff xxxxx"
  ]

doTests :: [String]
doTests = [
  "y = do {x1}",
  "y = do {x1; x2}",
  "y = do {x1; x2; x3}",
  "y = do {x1 <- m1; x2}",
  "y = do {(x1, x2) <- m1; x1 + x2}",
  "y = do {x1 <- m1; x2 <- f x1; g x2}",
  "y = do {let {x = 1}; x2 <- x; f x2}"
  ]

tupleTests :: [String]
tupleTests = [
    "y = ()"
  , "y = (x,) 2"
  , "y = (1,2,3)"
  , "y = (,)"
  , "y = (,x)"
  , "y = (x,)"
  ]

listTests :: [String]
listTests = [
  "y = []",
  "y = [1]",
  "y = [1,2]",
  "y = [1,2,3]"
  ]

caseTests :: [String]
caseTests = [
  "y = case x of {0 -> 1; 2 -> 3}"
  , "y = case f x of {0 -> 1; 2 -> 3}"
  , "y = case x of {Foo a -> a}"
  , "y = case x of {Foo a -> f a; Bar a -> f a}"
  , "y = case x of {F x -> x; G x -> x}"
  , "y = case x of {F -> 0; G -> 1}"
  , "single [] = []\n\
  \single (x:xs) = [x] : single xs"  
  -- ,
  -- "z = case x of {0 -> 1; y -> y}",
  -- "y x = case f x of {0 -> x; Foo x -> x}"
  ]

guardTests :: [String]
guardTests = [
  "y x\n\
  \  | x == 0 = 1",
  "y x\n\
  \  | x == 0 = 1\n\
  \  | otherwise = 2"
  ]

patternTests :: [String]
patternTests = [
  "Foo 3 = 4",

  "Foo _ x = 3",

  "y (F x) = x",

  "y = let {g = 3; F x y = h g} in x y",

  "y = let {F x y = 3} in x y",

  "y = let F x y = g in x y",

  "F x = g x",
  "Foo (Bar x) (Baz y) = f 1 2 x y",
  "Foo x y = f 1 y x",

  "t@(Foo x) = 3",

  "(x, y) = 3",
  "(x, y, z) = 3",

  "[x] = 1",
  "[x, y] = 2",
  "[x, y, z] = 3",

  "y = let {t@(_,_) = (3,4)} in t + 3",

  -- TODO There is no bind text box for n2
  "n1@(n2@(x,y)) = f n1 n2 x y",

  "n0@(Foo n1@(Bar x) n2@(Baz y)) = f n0 n1 x n2 y",
  "baz = case 0 of {n0@(Foo n1@(Bar x) n2@(Baz y)) -> f n0 n1 x n2 y}",
  "func n0@(Foo n1@(Bar x) n2@(Baz y)) = f n0 n1 x n2 y",

  "y = let {(x, y) = (1,2)} in x + y",
  "y = let {(x, y) = (1,2); (z, w) = x; (m, g) = y} in foo x y z w m g",
  "(x:y) = 2",

  -- test labelled ports
  "Foo x1 x2 = 4"
  , "mirrorTree Empty = Empty \n\
    \mirrorTree (Node a l r) = Node a (mirrorTree r) (mirrorTree l)"
  ]

lambdaTests :: [String]
lambdaTests = [
  "y = (\\x -> if True then 0 else 1) 3",
  "y = (\\x -> 99) x",
  "y = (\\x -> (\\x -> 2) x)",
  "y = (\\x -> (\\x -> f 2) x)",
  "y = (\\x -> (\\x -> x) x)",
  "y = (\\x -> (\\x -> (\\x -> x) x) x)",
  "y = (\\x -> (\\x -> (\\x -> x)))",
  "y = (\\y -> y)",
  "y = (\\x1 -> (\\x2 -> (\\x3 -> x1 x2 x3)))",
  "y = f1 a where f1 b = f2 c where f2 d = f3 e",
  "y x = (\\z -> x)",
  "y x = y x",
  "y x = g y y",
  "y f x = f x",
  "y x = x y",
  "y x1 x2 = f x1 x3 x2",
  "y x1 x2 = f x1 x2",
  "y x = f x1 x2",
  "y (-1) = 2",
  "y (-1) = 2;y 1 = 3;",
  "y 1 = 0",
  "y x = z 3 where z = f x y",
  "y (Foo x) = x; y (Bar x) = 3"  -- test multiple matches
  ]

letTests :: [String]
letTests = [
  "letInExpression = let a = x in a",
  "y x = f x x",
  "y x1 = let x2 = f x1 in x2 x1",

  "y = g $ f y",
  "y = let {a = f b; b = g a} in b",

  "y = let {a= 1; x = let {a = 27; x = f a 2} in x} in x",
  "y = let {a = b; b = a; d = f a} in d"
  -- , "y = let {a = b; b = a} in a" -- loop
  -- , "y = let x = x in x" -- loop
  ]

operatorTests :: [String]
operatorTests = [
  -- right section
  "y = map (++ 1) 3",
  -- left section
  "y = (1 +) 2"
  ]

otherTests :: [String]
otherTests = [
  "y = f 1 'c' 2.3 \"foobar\"",
  "fact x = if (x == 0) then 1 else (x * fact (x - 1))",
  "fibs = cons 0 (cons 1 (zipWith (+) fibs (tail fibs)))",
  "y x = if x then (if z then q else x) else w",
  "y x1 x2 x3 = if f x1 then g x2 else h x3",
  "y x1 x2 x3 = if x1 then x2 else x3",
  "y = if b then x else n",
  "y2 = f x1 x2 x3 x4",
  "y = x",
  "y x = y x",
  "y = f 3 y",
  "y = f x",
  "y = f (g x1 x2) x3",
  "y = (f x1 x2) (g x1 x2)",
  "y = Foo.bar",
  "y x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 = f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10",
  -- test that whitespace is preserved
  "y = \" foo  bar   baz    \""
  ]

multiWayIfTests :: [String]
multiWayIfTests = [
  "y = if | x == 0 -> 1"
  , "y = if\n\
  \  | x == 0 -> 1\n\
  \  | otherwise -> 2"
  , "y = if\n\
  \  | x == 0 -> if {| y > z -> 2; | somethingElse -> 3} \n\
  \  | otherwise -> 2"
  ]

listCompTests :: [String]
listCompTests = [
  "myfilter f xs = [x | x <- xs, f x]"
  ,
  "mymap f xs = [f x | x <- xs]"
  ,
  "multipleGenerators = [(i,j) | i <- [1,2],\n\
  \                                 j <- [1..4] ]"
  ,
  "nested = take 5 [ [ (i,j) | i <- [1,2] ] | j <- [1..] ]"
  ,
  "booleanGuards = take 10 [ (i,j) | i <- [1..], \n\
  \                                  j <- [1..i-1], \n\
  \                                  gcd i j == 1 ]"
  ,
  "localLet = take 10 [ (i,j) | i <- [1..], \n\
  \                             let k = i*i, \n\
  \                             j <- [1..k] ]"
  ,
  "patternFilter xs = [l | (Left l) <- xs]"
  ]

otherInLambdaTest :: [String]
otherInLambdaTest = [
  "undefinedFunInLambda = \\x y -> (y, f x)"
  ,"lambdaWIfUnused = \\a1 a2 -> if cond then v1 else v2"
  ,"lambdaWIfValues = \\a1 a2 a3-> if a1 then a2 else a3"
  ,"lambdaWEnumUnused = \\a1 a2 -> [1..]"
  ,"lambdaWEnumValues = \\a1 a2 -> [a1..a2]"
  ,"lambdaWLetUnused = \\a1 a2 -> let x = 1 in  x "
  ,"lambdaWLetValue = \\a1 a2 -> let x = 1 in  a1 + x"
  ,"lambdaWLetDescription = \\a1 a2 -> let x = a1 in  v1 + x"
  ,"lambdaWLetBoth = \\a1 a2 -> let x = a1 in  a2 + x"
  ,"lambdaWListComp = \\xs -> [x | x <- xs] "
  ,"lambdaWCaseCondAndValue = \\a1 a3-> case a1 of True -> a3"
  ,"lambdaWCaseUnused = \\a1 a2 -> case v1 of V2 -> v3"
  -- not supported
  -- , "lambdaWCaseMixed = \\a1 a2 -> case a1 of \n\ 
  -- \  a2 -> v1\n\
  -- \  c1 -> v2"
  ,
  "y = lambdaWGuardsValue v1 v2 where \n\
  \  lambdaWGuardsValue = \\ aa1 aa2->\n\
  \    let  \n\
  \      guards\n\
  \        | cond       = aa1\n\
  \        | otherwise = aa2\n\
  \    in  guards"
  ,
  "lambdaWGuardsCondition = \\ aa1 aa2 ->\n\
  \  let\n\
  \    guards\n\
  \      | aa1     = v1\n\
  \      | aa2     = v2\n\
  \      | otherwise = v3\n\
  \  in  guards"
  ,
  "appliedLambdaWGuards = lambdaWGuardsValue v1 v2 where \n\
  \  lambdaWGuardsValue = \\ aa1 aa2->\n\
  \    let  \n\
  \      guards\n\
  \        | cond       = aa1\n\
  \        | otherwise = aa2\n\
  \    in  guards"
  ,
  "lambdaWAppliedGuards = \\ aa1 aa2->\n\
  \    let  \n\
  \      guards a1 a2\n\
  \        | cond       = a1\n\
  \        | otherwise = a2\n\
  \    in  guards aa1 aa2"
  ,
  "lambdaWFunctionDefValues = \\a1 a2 ->\n\
  \  let\n\
  \    functionDef a1p1 a2p1 = a1\n\
  \    functionDef a1p2 a2p2 = a2\n\
  \  in  functionDef"
  ]
testDecls :: [String]
testDecls = mconcat [
  simpleTests
  , enumTest
  , composeTests
  , nestedTests
  , doTests
  , caseTests
  ,
  lambdaTests
  , guardTests
  , patternTests
  , specialTests
  , tupleTests
  , listTests
  , letTests
  , operatorTests
  , otherTests
  , typeSigTests
  , dataDeclTests
  , multiWayIfTests
  , listCompTests
  ,
  otherInLambdaTest
  ]


translateStringToDrawing :: DrawingBackend b =>
  String
  -> IO (Drawing b)
translateStringToDrawing s = do
  putStrLn $ "Translating string: " ++ s
  let
    decl = customParseDecl s
    simpDecl = stringToSimpDecl s
    collapsedGraph = translateDeclToCollapsedGraph decl
    syntaxGraph = translateStringToSyntaxGraph s
    fglGraph = syntaxGraphToFglGraph syntaxGraph
  let
    printAction = do
      putStrLn "Exts.Decl:"
      print decl
      putStrLn "SimpDecl:"
      print simpDecl
      putStrLn "SyntaxGraph:"
      putStr $ prettyShowSyntaxGraph syntaxGraph
      putStrLn "FGL Graph:"
      ING.prettyPrint fglGraph
      putStrLn "Collapsed Graph:"
      ING.prettyPrint collapsedGraph
      putStr "\n\n"
  if False then printAction else pure ()  -- Supress unused printAction warning
  declarationDiagrams <- renderIngSyntaxGraph dummyColorStyle (collapsedGraph, collapsedGraph)
  pure $ clearValue declarationDiagrams

visualTranslateTests :: (HasCallStack, DrawingBackend b)
                     => IO (Drawing b)
visualTranslateTests = do
  drawings <- traverse translateStringToDrawing testDecls
  let
    textDrawings = fmap (alignL . textBox) testDecls
    vCattedDrawings = vsep 1 $ zipWith (===) (fmap alignL drawings) textDrawings
  pure vCattedDrawings

textBox :: DrawingBackend b =>
  String -> Drawing b
textBox = multilineComment dummyColorStyle