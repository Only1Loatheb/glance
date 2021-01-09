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
import           Data.Maybe()
import qualified Language.Haskell.Exts as Exts
import           Data.List( sortBy )
import Data.List.Split as Split
import           Data.Function( on )

import Rendering(renderIngSyntaxGraph)
import           Types  (
  QueryableDrawing
  , DrawingBackend
  , SrcRef
  , NodeQueryValue(..)
  , ModuleGraphs
  , QueryValue(..)
  , DeclQueryValue(..)
  , NodeQueryValue(..)
  , AnnotatedFGR
  , SourceCode
  , SyntaxGraph(..)
  , backgroundC
  , ColorStyle
  )

import PartialView (neighborsSubgraph)

import TextBox (multilineComment, sourceCodeDiagram)
import CollapseGraph(syntaxGraphToCollapsedGraph, syntaxGraphToLessCollapsedGraph)
-- before loop
diagramFromModule :: DrawingBackend b =>
  (SrcRef -> SourceCode) -> ColorStyle -> Bool -> ModuleGraphs -> QueryableDrawing b
diagramFromModule getCodeFragment colorStyle includeComments (declSpansAndGraphs, comments) = moduleDiagram where
  selectedComments = selectComments includeComments comments
  moduleDiagram = getModuleDiagram getCodeFragment colorStyle declSpansAndGraphs selectedComments

selectComments :: Bool -> [Exts.Comment] -> [Exts.Comment]
selectComments includeComments comments = if includeComments then comments else []

getModuleDiagram :: DrawingBackend b
  => (SrcRef -> SourceCode) -> ColorStyle -> [(SrcRef, SyntaxGraph)] -> [Exts.Comment] -> QueryableDrawing b
getModuleDiagram getCodeFragment colorStyle declSpansAndGraphs selectedComments = diagram where
  chunks = getDeclChunks declSpansAndGraphs
  diagram = placeDiagrams $ map (declIconDiagram getCodeFragment colorStyle selectedComments) chunks 

diagramSeparation :: Fractional p => p
diagramSeparation = 1.0

placeDiagrams :: DrawingBackend b
  => [QueryableDrawing b]
  -> QueryableDrawing b
placeDiagrams diagrams = finalDia where
  finalDia = Dia.vsep diagramSeparation diagrams

declIconDiagram :: DrawingBackend b
  => (Exts.SrcSpan -> String) 
  -> ColorStyle 
  -> [Exts.Comment] 
  -> [(Exts.SrcSpan, SyntaxGraph)] 
  -> QueryableDrawing b
declIconDiagram getCodeFragment colorStyle comments [(srcRefBefore,_),(srcRef, graph), (srcRefAfter,_)] = diagram where
  diagram = Dia.value queryValue
    $ (multilineComment colorStyle . head . lines . getCodeFragment) srcRef
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
declDiagram :: DrawingBackend b => ColorStyle -> DeclQueryValue -> IO (QueryableDrawing b)
declDiagram colorStyle declQV@(DeclQueryValue _ declGraph _ _) = do
  let 
    (topComments, bottomComments) = viewDiagramBase colorStyle declQV
    collapsedDeclGraph = syntaxGraphToCollapsedGraph declGraph
  declDia <- renderIngSyntaxGraph colorStyle (collapsedDeclGraph, collapsedDeclGraph)
  let diagram = topComments Dia.=== Dia.alignL declDia Dia.=== bottomComments
  pure diagram

viewDiagramBase :: DrawingBackend b => ColorStyle -> DeclQueryValue -> (QueryableDrawing b, QueryableDrawing b)
viewDiagramBase colorStyle (DeclQueryValue _ _ commentsBefore commentsAfter) = (topComments, bottomComments) where
  topComments = placeDiagrams $ map (commentToDiagram colorStyle) commentsBefore
  bottomComments = placeDiagrams $ map (commentToDiagram colorStyle) commentsAfter

nodeDiagram :: DrawingBackend b => ColorStyle -> DeclQueryValue -> NodeQueryValue -> IO (QueryableDrawing b)
nodeDiagram colorStyle declQV@(DeclQueryValue _ declGraph _ _) (NodeQueryValue _ name) = do
  let
    collapsedDeclGraph = syntaxGraphToLessCollapsedGraph declGraph
    (topComments, bottomComments) = viewDiagramBase colorStyle declQV
    viewGraph = neighborsSubgraph name collapsedDeclGraph
  nodeDia <- renderIngSyntaxGraph colorStyle (collapsedDeclGraph, viewGraph)
  let diagram = topComments Dia.=== Dia.alignL nodeDia Dia.=== bottomComments
  pure diagram

commentToDiagram :: DrawingBackend b
  => ColorStyle
  -> Exts.Comment
  -> QueryableDrawing b
commentToDiagram colorStyle (Exts.Comment _ _ c) = Dia.value mempty $ multilineComment colorStyle c

-- Not queryable diagram
staticDiagramFromModule :: DrawingBackend b =>
  Bool -> ModuleGraphs -> ColorStyle -> IO(QueryableDrawing b)
staticDiagramFromModule includeComments (declSpansAndGraphs, comments) colorStyle = do
  let 
    (declarationSpans, graphs) = unzip declSpansAndGraphs
    fullGraphs = map (moduleGraphsToViewGraphs . syntaxGraphToCollapsedGraph) graphs
  --print drawingsGraphs
  declarationDiagrams <- traverse (renderIngSyntaxGraph colorStyle) fullGraphs
  let
    spanAndDeclarations = zip declarationSpans declarationDiagrams
    selectedComments = selectComments includeComments comments
    spanAndcomments = map (commentToSpanAndDiagram colorStyle) selectedComments
    spanAndDiagrams = spanAndcomments ++ spanAndDeclarations
    moduleDiagram = composeDiagrams spanAndDiagrams
    moduleDiagramWithBg = Dia.bg (backgroundC colorStyle) moduleDiagram
  --print comments
  pure moduleDiagramWithBg

composeDiagrams :: DrawingBackend b
  => [(Exts.SrcSpan, QueryableDrawing b)]
  -> QueryableDrawing b
composeDiagrams diagrams = finalDiagram where
  sortedDiagarms = snd <$> sortBy (compare `on` fst) diagrams
  finalDiagram = placeDiagrams sortedDiagarms

commentToSpanAndDiagram :: DrawingBackend b
  => ColorStyle
  -> Exts.Comment
  -> (Exts.SrcSpan, QueryableDrawing b)
commentToSpanAndDiagram colorStyle comment@(Exts.Comment _ srcSpan _) = (srcSpan, commentToDiagram colorStyle comment)

moduleGraphsToViewGraphs :: AnnotatedFGR -> (AnnotatedFGR, AnnotatedFGR)
moduleGraphsToViewGraphs graph = (graph, graph) 

addSourceCodeDiagram :: DrawingBackend b
  => ColorStyle
  -> QueryableDrawing b
  -> SourceCode
  -> QueryableDrawing b
addSourceCodeDiagram colorStyle diagram codeString = diagramWithsourceCode where
  sourceCodeDia = Dia.value mempty $ sourceCodeDiagram  codeString colorStyle
  diagramWithsourceCode = diagram Dia.=== sourceCodeDia