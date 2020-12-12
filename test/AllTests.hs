module Main (main) where
import Prelude hiding (return)

import Diagrams.Backend.SVG.CmdLine(B)
import Diagrams.Prelude hiding ((#), (&))

import GHC.Stack(HasCallStack)
import Test.HUnit

import SVGrender(customRenderSVG)

import VisualTests(visualTranslateTests)
import VisualGraphAlgorithmTests(visualCollapseTests)

import SyntaxGraphDirectTests(syntaxGraphDirectTests)
import DrawingColors (dummyColorStyle)
import Types (ColorStyle'(backgroundC))

drawingsAndNames :: [(String, IO (Diagram B))]
drawingsAndNames =
  [ ("visual-tests", visualTranslateTests)
  , ("collapse-tests", visualCollapseTests)
  ]

renderDrawings :: HasCallStack => [(String, IO (Diagram B))] -> IO ()
renderDrawings = mapM_ saveDrawing where
  saveDrawing (name, drawingMaker) = do
    dia <- drawingMaker
    let diaWithBg = bg (backgroundC dummyColorStyle) dia
    -- TODO Replace string concatenation with proper path manipulation functions.
    customRenderSVG ("test/test-output/" ++ name ++ ".svg") widthScale diaWithBg
      where widthScale = 1

main :: HasCallStack => IO ()
--main = print "Hello world"
main = do
  --  ING.prettyPrint singleApplyGraph
  renderDrawings drawingsAndNames
  _ <- runTestTT syntaxGraphDirectTests
  pure ()
