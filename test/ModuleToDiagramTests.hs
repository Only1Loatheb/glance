module ModuleToDiagramTests(
  moduleToDiagramTests
  ) where

import Test.HUnit
import qualified Language.Haskell.Exts as Exts
import ModuleToDiagram as MTD

moduleToDiagramTests :: Test
moduleToDiagramTests = TestList[
  TestLabel "ModuleToDiagramTests.hs" selectGraphTests
  ]

fileName :: String
fileName = "./examples/simpleFunctions.hs"

selectGraphTests :: Test
selectGraphTests = TestList [
  TestCase (assertEqual "selectFromOneElement"
      (Exts.SrcSpan fileName 1 0 3 0, 1 :: Integer)
      (
      let
        srcRef = Exts.SrcSpan fileName 2 0 2 0
        declSpansAndGraphs = [
          (Exts.SrcSpan fileName 1 0 3 0, 1)
          ]
      in 
        fst $ MTD.selectGraph srcRef declSpansAndGraphs
      )
    )
  , TestCase (assertEqual "selectFirstElement" 
      (Exts.SrcSpan fileName 1 0 3 0, 1 :: Integer)
      (
      let
        srcRef = Exts.SrcSpan fileName 2 0 2 0
        declSpansAndGraphs = [
          (Exts.SrcSpan fileName 1 0 3 0, 1)
          , (Exts.SrcSpan fileName 4 0 6 0, 2)
          , (Exts.SrcSpan fileName 7 0 9 0, 3)
          ]
      in 
        fst $ MTD.selectGraph srcRef declSpansAndGraphs
      )
    )
  , TestCase (assertEqual "selectMiddleElement" 
      (Exts.SrcSpan fileName 4 0 6 0, 2:: Integer)
      (
      let
        srcRef = Exts.SrcSpan fileName 5 0 5 0
        declSpansAndGraphs = [
          (Exts.SrcSpan fileName 1 0 3 0, 1)
          , (Exts.SrcSpan fileName 4 0 6 0, 2)
          , (Exts.SrcSpan fileName 7 0 9 0, 3)
          ]
      in 
        fst $ MTD.selectGraph srcRef declSpansAndGraphs
      )
    )
  , TestCase (assertEqual "selectLastElement"
      (Exts.SrcSpan fileName 7 0 9 0, 3 :: Integer)
      (
      let
        srcRef = Exts.SrcSpan fileName 8 0 8 0
        declSpansAndGraphs = [
          (Exts.SrcSpan fileName 1 0 3 0, 1)
          , (Exts.SrcSpan fileName 4 0 6 0, 2)
          , (Exts.SrcSpan fileName 7 0 9 0, 3)
          ]
      in 
        fst $ MTD.selectGraph srcRef declSpansAndGraphs
      )
    )
  ]
  -- , Exts.SrcSpan fileName 4 0 5 0
  -- , Exts.SrcSpan fileName 1 0 1 0