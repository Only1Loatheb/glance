module Main (main) where
import Prelude hiding (return)

import Diagrams.Backend.SVG.CmdLine(B)
import Diagrams.Prelude hiding ((#), (&))

import GHC.Stack(HasCallStack)
import Test.HUnit

import SVGrender(customRenderSVG)

import VisualTests(visualTranslateTests)
import VisualGraphAlgorithmTests(visualCollapseTests)

import SyntaxGraphComparisonTests(syntaxGraphComparisonTests)
import SyntaxGraphDirectTests(syntaxGraphDirectTests)

{-# ANN module "HLint: ignore Unnecessary hiding" #-}

drawingsAndNames :: [(String, IO (Diagram B))]
drawingsAndNames =
  [ ("visual-tests", visualTranslateTests)
  , ("collapse-tests", visualCollapseTests)
  ]

renderDrawings :: HasCallStack => [(String, IO (Diagram B))] -> IO ()
renderDrawings = mapM_ saveDrawing where
  saveDrawing (name, drawingMaker) = do
    dia <- drawingMaker
    -- TODO Replace string concatenation with proper path manipulation functions.
    customRenderSVG ("test/test-output/" ++ name ++ ".svg") widthScale dia 
      where widthScale = 1

main :: HasCallStack => IO ()
--main = print "Hello world"
main = do
  --  ING.prettyPrint singleApplyGraph
  renderDrawings drawingsAndNames
  _ <- runTestTT syntaxGraphDirectTests
  _ <- runTestTT syntaxGraphComparisonTests
  pure ()
