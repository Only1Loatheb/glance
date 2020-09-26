module Main (main) where
import Prelude hiding (return)

import Diagrams.Backend.SVG.CmdLine(B)
import Diagrams.Prelude hiding ((#), (&))

import GHC.Stack(HasCallStack)
import Test.HUnit

import IconToSymbolDiagram(colorScheme, ColorStyle(..))
import Util(customRenderSVG)

import SyntaxGraphTests(allUnitTests)
import VisualGraphAlgorithmTests(visualCollapseTests)
import VisualTests(visualTranslateTests)
import ModuleToDiagramTests(moduleToDiagramTests)
import SyntaxToGraphTests(syntaxToGraphTests)

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
    customRenderSVG ("test/test-output/" ++ name ++ ".svg") (mkWidth 700) (bgFrame 1 (backgroundC colorScheme) dia)

main :: HasCallStack => IO ()
--main = print "Hello world"
main = do
  --  ING.prettyPrint singleApplyGraph
  renderDrawings drawingsAndNames
  _ <- runTestTT allUnitTests
  _ <- runTestTT moduleToDiagramTests
  _ <- runTestTT syntaxToGraphTests
  pure ()
