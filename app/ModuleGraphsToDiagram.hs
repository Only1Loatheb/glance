{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module ModuleGraphsToDiagram(
  diagramFromModule
  , staticDiagramFromModule
  , declDiagram
  , nodeDiagram
  ) where
-- import Prelude hiding (return)

-- Note: (#) and (&) are hidden in all Glance source files, since they would
-- require a special case when translating when Glance is run on its own source
-- code.
import qualified Diagrams.Prelude as Dia hiding ((#), (&))
import qualified Data.Map as Map
import           Data.Maybe
import qualified Language.Haskell.Exts as Exts
import           Data.List( sortBy )
import Data.List.Split as Split
import           Data.Function( on )

import IconToSymbolDiagram(
  ColorStyle(..)
  , colorScheme
  , multilineComment
  , sourceCodeDiagram
  )
import Rendering(renderIngSyntaxGraph)
import CollapseGraph(translateModuleToCollapsedGraphs)
import           Types  (
  SpecialDiagram
  , SpecialQDiagram
  , SpecialBackend
  , NamedIcon
  , SrcRef(..)
  , NodeQueryValue(..)
  , ModuleGraphs
  , ViewGraphs
  , QueryValue(..)
  , DeclQueryValue(..)
  , NodeQueryValue(..)
  , AnnotatedFGR(..)
  )

import PartialView (neighborsSubgraph)

import TextBox (multilineComment)
-- before loop
diagramFromModule :: SpecialBackend b Double =>
  Bool -> ModuleGraphs -> SpecialQDiagram b Double
diagramFromModule includeComments (declSpansAndGraphs, comments) = moduleDiagram where
  selectedComments = selectComments includeComments comments
  moduleDiagram = getModuleDiagram declSpansAndGraphs selectedComments

selectComments :: Bool -> [Exts.Comment] -> [Exts.Comment]
selectComments includeComments comments = if includeComments then comments else []

getModuleDiagram :: SpecialBackend b Double
  => [(SrcRef, AnnotatedFGR)] -> [Exts.Comment] -> SpecialQDiagram b Double
getModuleDiagram declSpansAndGraphs selectedComments =
  composeDiagramsInModule 
    $ map (declIconDiagram selectedComments) declSpansAndGraphs

diagramSeparation :: Fractional p => p
diagramSeparation = 1.0

composeDiagramsInModule :: SpecialBackend b Double
  => [SpecialQDiagram b Double]
  -> SpecialQDiagram b Double
composeDiagramsInModule diagrams = finalDia where
  finalDia = Dia.vsep diagramSeparation diagrams

declIconDiagram comments (span, graph) = diagram where
  diagram = Dia.value queryValue
    $ multilineComment $ show span
  queryValue = [DeclQv span $ DeclQueryValue graph comments []]

-- in loop 
declDiagram declQV@(DeclQueryValue declGraph commentsBefore commentsAfter) = do
  let (topComments, bottomComments) = viewDiagramBase declQV
  declDia <- renderIngSyntaxGraph "" (declGraph, declGraph)
  let diagram = topComments Dia.===  (Dia.alignL declDia) Dia.=== bottomComments
  pure (diagram)

viewDiagramBase (DeclQueryValue _ commentsBefore commentsAfter) = (topComments, bottomComments) where
  topComments = composeDiagramsInModule $ map commentToDiagram commentsBefore
  bottomComments = composeDiagramsInModule $ map commentToDiagram commentsAfter

nodeDiagram declQV@(DeclQueryValue declGraph commentsBefore commentsAfter) (NodeQueryValue name) = do
  let (topComments, bottomComments) = viewDiagramBase declQV
  let viewGraph = neighborsSubgraph name declGraph
  nodeDia <- renderIngSyntaxGraph "" (declGraph, viewGraph)
  let diagram = topComments Dia.=== (Dia.alignL nodeDia) Dia.=== bottomComments
  pure (diagram)

commentToDiagram :: SpecialBackend b Double
  => Exts.Comment
  -> SpecialQDiagram b Double
commentToDiagram (Exts.Comment _ _ c) = Dia.value mempty $ multilineComment  c

-- Not queryable diagram
staticDiagramFromModule :: SpecialBackend b Double =>
  Bool -> ModuleGraphs -> IO(SpecialQDiagram b Double)
staticDiagramFromModule includeComments (declSpansAndGraphs, comments) = do
  let 
    (declarationSpans, graphs) = unzip declSpansAndGraphs
    fullGraphs = map moduleGraphsToViewGraphs graphs
  --print drawingsGraphs
  declarationDiagrams <- traverse (renderIngSyntaxGraph "") fullGraphs
  let
    spanAndDeclarations = zip declarationSpans declarationDiagrams
    selectedComments = selectComments includeComments comments
    spanAndcomments = fmap commentToSpanAndDiagram selectedComments
    spanAndDiagrams = spanAndcomments ++ spanAndDeclarations
    moduleDiagram = composeDiagrams spanAndDiagrams
  --print comments
  pure (moduleDiagram)

composeDiagrams :: SpecialBackend b Double
  => [(Exts.SrcSpan, SpecialQDiagram b Double)]
  -> (SpecialQDiagram b Double)
composeDiagrams diagrams = finalDiagram where
  sortedDiagarms = snd <$> sortBy (compare `on` fst) diagrams
  finalDiagram = composeDiagramsInModule sortedDiagarms

commentToSpanAndDiagram :: SpecialBackend b Double
  => Exts.Comment
  -> (Exts.SrcSpan, SpecialQDiagram b Double)
commentToSpanAndDiagram (Exts.Comment _ srcSpan c) = (srcSpan, Dia.value mempty $ multilineComment  c)

moduleGraphsToViewGraphs :: AnnotatedFGR -> (AnnotatedFGR, AnnotatedFGR)
moduleGraphsToViewGraphs graph = (graph, graph) 
