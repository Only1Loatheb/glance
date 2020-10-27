{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module ModuleGraphsToDiagram(
  diagramFromModule
  , staticDiagramFromModule
  , declDiagram
  , nodeDiagram
  , addSourceCodeDiagram
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
  , SourceCode
  , SyntaxGraph(..)
  )

import PartialView (neighborsSubgraph)

import TextBox (multilineComment)
import CollapseGraph(syntaxGraphToCollapsedGraph, syntaxGraphToLessCollapsedGraph)
-- before loop
diagramFromModule :: SpecialBackend b Double =>
  (SrcRef -> SourceCode) -> Bool -> ModuleGraphs -> SpecialQDiagram b Double
diagramFromModule getCodeFragment includeComments (declSpansAndGraphs, comments) = moduleDiagram where
  selectedComments = selectComments includeComments comments
  moduleDiagram = getModuleDiagram getCodeFragment declSpansAndGraphs selectedComments

selectComments :: Bool -> [Exts.Comment] -> [Exts.Comment]
selectComments includeComments comments = if includeComments then comments else []

getModuleDiagram :: SpecialBackend b Double
  => (SrcRef -> SourceCode) -> [(SrcRef, SyntaxGraph)] -> [Exts.Comment] -> SpecialQDiagram b Double
getModuleDiagram getCodeFragment declSpansAndGraphs selectedComments = diagram where
  chunks = getDeclChunks declSpansAndGraphs
  diagram = placeDiagrams $ map (declIconDiagram getCodeFragment selectedComments) chunks 

diagramSeparation :: Fractional p => p
diagramSeparation = 1.0

placeDiagrams :: SpecialBackend b Double
  => [SpecialQDiagram b Double]
  -> SpecialQDiagram b Double
placeDiagrams diagrams = finalDia where
  finalDia = Dia.vsep diagramSeparation diagrams

-- declIconDiagram :: [Exts.Comment] -> [(Exts.SrcSpan, b)] SpecialQDiagram b Double
declIconDiagram getCodeFragment comments [(srcRefBefore,_),(srcRef, graph), (srcRefAfter,_)] = diagram where
  diagram = Dia.value queryValue
    $ ( multilineComment . head . lines . getCodeFragment) srcRef
  queryValue = [DeclQv $ DeclQueryValue srcRef graph commentsBefore commentsAfter]
  commentsBefore = filterComments comments (srcRefBefore, srcRef)
  commentsAfter = filterComments comments (srcRef, srcRefAfter)

getDeclChunks :: [(Exts.SrcSpan, b)]
  -> [[(Exts.SrcSpan, b)]]
getDeclChunks declSpansAndGraphs 
  = Split.divvy chunkSize chunkOffset paddedDeclSpansAndGraphs where
    chunkSize = 3
    chunkOffset = 1
    paddedDeclSpansAndGraphs = headPadding : declSpansAndGraphs ++ [tailPadding] where
      (headPadding, tailPadding) = getPadding declSpansAndGraphs

getPadding :: [(Exts.SrcSpan, b1)]
                -> ((Exts.SrcSpan, b2), (Exts.SrcSpan, b2))
getPadding declSpansAndGraphs = (headPadding, tailPadding) where
  fileName = Exts.srcSpanFilename $ fst $ head declSpansAndGraphs
  paddingError = error "padding used as a graph"
  headPadding = (srcRefBefere, paddingError) where
    srcRefBefere = Exts.SrcSpan fileName 0 0 0 0
  tailPadding = (srcRefAfter, paddingError) where
    srcRefAfter = Exts.SrcSpan fileName maxBound maxBound maxBound maxBound

filterComments :: [Exts.Comment]
                    -> (Exts.SrcSpan, Exts.SrcSpan) -> [Exts.Comment]
filterComments comments (srcSpanBefore, srcSpanAfter) = nerbyComments where
  nerbyComments = filter (\((Exts.Comment _ srcSpan _))-> srcSpan > srcSpanBefore && srcSpan < srcSpanAfter) comments

-- in loop 
declDiagram declQV@(DeclQueryValue _ declGraph commentsBefore commentsAfter) = do
  let 
    (topComments, bottomComments) = viewDiagramBase declQV
    collapsedDeclGraph = syntaxGraphToCollapsedGraph declGraph
  declDia <- renderIngSyntaxGraph "" (collapsedDeclGraph, collapsedDeclGraph)
  let diagram = topComments Dia.===  (Dia.alignL declDia) Dia.=== bottomComments
  pure (diagram)

viewDiagramBase (DeclQueryValue _ _ commentsBefore commentsAfter) = (topComments, bottomComments) where
  topComments = placeDiagrams $ map commentToDiagram commentsBefore
  bottomComments = placeDiagrams $ map commentToDiagram commentsAfter

nodeDiagram declQV@(DeclQueryValue _ declGraph commentsBefore commentsAfter) (NodeQueryValue _ name) = do
  let
    collapsedDeclGraph = syntaxGraphToLessCollapsedGraph declGraph
    (topComments, bottomComments) = viewDiagramBase declQV
    viewGraph = neighborsSubgraph name collapsedDeclGraph
  nodeDia <- renderIngSyntaxGraph "" (collapsedDeclGraph, viewGraph)
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
    fullGraphs = map (moduleGraphsToViewGraphs . syntaxGraphToCollapsedGraph) graphs
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
  finalDiagram = placeDiagrams sortedDiagarms

commentToSpanAndDiagram :: SpecialBackend b Double
  => Exts.Comment
  -> (Exts.SrcSpan, SpecialQDiagram b Double)
commentToSpanAndDiagram (Exts.Comment _ srcSpan c) = (srcSpan, Dia.value mempty $ multilineComment  c)

moduleGraphsToViewGraphs :: AnnotatedFGR -> (AnnotatedFGR, AnnotatedFGR)
moduleGraphsToViewGraphs graph = (graph, graph) 

-- addSourceCodeDiagram :: SpecialBackend b Double
--   => SpecialQDiagram b Double
--   -> SourceCode
--   -> SpecialQDiagram b Double
addSourceCodeDiagram diagram codeString = diagramWithsourceCode where
  sourceCodeDia = Dia.value mempty $ sourceCodeDiagram codeString
  diagramWithsourceCode = (diagram Dia.===  sourceCodeDia)