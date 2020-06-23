{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module VisualGraphAlgorithmTests (
  visualCollapseTests
  ) where

import Diagrams.Prelude hiding ((#), (&))

import qualified Data.GraphViz as GV
import qualified Diagrams.TwoD.GraphViz as DiaGV
import qualified Data.GraphViz.Attributes.Complete as GVA

import qualified Data.Graph.Inductive as ING
import qualified Data.Graph.Inductive.PatriciaTree as FGR

import Types(SpecialDiagram, SpecialBackend, SyntaxNode(..), SgNamedNode
            , NodeInfo(..), Named(..), Embedder(..))
import SimpSyntaxToSyntaxGraph(translateStringToSyntaxGraph)
import CollapseGraph(annotateGraph, collapseAnnotatedGraph, syntaxGraphToFglGraph)
import Rendering(customLayoutParams)
import TextBox(coloredTextBox)

{-# ANN module "HLint: ignore Unnecessary hiding" #-}

prettyPrintSyntaxNode :: SyntaxNode -> String
-- TODO Re-enable
-- prettyPrintSyntaxNode (Embedder namedNodesAndEdges _)
--   = concatMap printNameAndEdge namedNodesAndEdges
--   where
--     printNameAndEdge (namedNode, edge)
--       = "(" ++ show namedNode ++ "," ++ printEdge edge ++ ")"
--     printEdge (Edge _ (NameAndPort n1 _, NameAndPort n2 _)) = show (n1, n2)
prettyPrintSyntaxNode = show

renderFglGraph :: SpecialBackend b Double
               => FGR.Gr SgNamedNode e
               -> IO (SpecialDiagram b Double)
renderFglGraph fglGraph = do
  layedOutGraph <- DiaGV.layoutGraph' layoutParams GVA.Neato fglGraph
  pure $ DiaGV.drawGraph
    nodeFunc
    --(\_ _ _ _ _ p -> lc white $ stroke p)
    -- TODO Draw some type of arrow if point1 == point2
    (\_ point1 _ point2 _ _ ->  if point1 == point2
      then mempty
      else lcA (withOpacity white 0.7)
           $ arrowBetween'
           (shaftStyle %~ lwG 0.5 $ headLength .~ global 1.5 $ with)
           (scaleFactor *^ point1)
           (scaleFactor *^ point2))
    (ING.nmap (fmap (fmap emNode)) layedOutGraph)
  where
    scaleFactor = 0.3
    nodeFunc (Named name syntaxNode) point
      = place (coloredTextBox
                white
                (show name ++ prettyPrintSyntaxNode syntaxNode)
                {- :: Diagram B -})
      (scaleFactor *^ point)
    layoutParams :: GV.GraphvizParams Int v e Int v
    layoutParams = customLayoutParams{
      GV.fmtNode = nodeAttribute
    }
    nodeAttribute :: (Int, l) -> [GV.Attribute]
    nodeAttribute _ =
      -- GVA.Width and GVA.Height have a minimum of 0.01
      --[GVA.Width diaWidth, GVA.Height diaHeight]
      [GVA.Width 0.01, GVA.Height 0.01]

collapseTestStrings :: [String]
collapseTestStrings = [
  "y = x",
  "y = 1.0",
  "y = f x",
  "y = f x1 x2",
  "y = f (g x)",
  "y = g (\\x -> x)",
  "y = f $ g (\\x -> x)",
  "y = foo (3 + bazOf2) (8 * bazOf2) where bazOf2 = baz 2",
  "Foo x = 1",
  "Foo (Bar x) = 1",
  "Foo 1 x = 2",
  "Foo (Bar x) = f x",
  "y x = case x of {Just w -> (let (z,_) = w in z)}"
  ]

makeCollapseTest :: SpecialBackend b Double => String -> IO (SpecialDiagram b Double)
makeCollapseTest str = do
  before <- renderFglGraph fglGraph
  afterCollapse <- renderFglGraph (ING.nmap niVal collapsedGraph)
  pure $ vsep 1 [
    expressionText,
    beforeText,
    before,
    afterText,
    afterCollapse]
  where
    fglGraph = syntaxGraphToFglGraph $ translateStringToSyntaxGraph str
    collapsedGraph = collapseAnnotatedGraph $ annotateGraph fglGraph
    customTextBox = coloredTextBox white
    expressionText = alignL $ coloredTextBox white str -- :: Diagram B
    beforeText = alignL $ customTextBox "Before:" -- :: Diagram B
    afterText = alignL $ customTextBox "After:" -- :: Diagram B

-- TODO Make this work for many input strings
visualCollapseTests :: SpecialBackend b Double => IO (SpecialDiagram b Double)
visualCollapseTests = do
  drawings <- traverse makeCollapseTest collapseTestStrings
  pure $ vsep 1 drawings
