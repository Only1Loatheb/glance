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
  SpecialQDiagram
  , SpecialBackend
  , SrcRef
  , NodeQueryValue(..)
  , ModuleGraphs
  , QueryValue(..)
  , DeclQueryValue(..)
  , NodeQueryValue(..)
  , AnnotatedFGR
  , SourceCode
  , SyntaxGraph(..)
  )

import PartialView (neighborsSubgraph)

import TextBox (multilineComment, sourceCodeDiagram)
import CollapseGraph(syntaxGraphToCollapsedGraph, syntaxGraphToLessCollapsedGraph)
import DrawingColors (ColorStyle)
-- before loop
diagramFromModule :: SpecialBackend b Double =>
  (SrcRef -> SourceCode) -> ColorStyle Double -> Bool -> ModuleGraphs -> SpecialQDiagram b Double
diagramFromModule getCodeFragment colorStyle includeComments (declSpansAndGraphs, comments) = moduleDiagram where
  selectedComments = selectComments includeComments comments
  moduleDiagram = getModuleDiagram getCodeFragment colorStyle declSpansAndGraphs selectedComments

selectComments :: Bool -> [Exts.Comment] -> [Exts.Comment]
selectComments includeComments comments = if includeComments then comments else []

getModuleDiagram :: SpecialBackend b Double
  => (SrcRef -> SourceCode) -> ColorStyle Double -> [(SrcRef, SyntaxGraph)] -> [Exts.Comment] -> SpecialQDiagram b Double
getModuleDiagram getCodeFragment colorStyle declSpansAndGraphs selectedComments = diagram where
  chunks = getDeclChunks declSpansAndGraphs
  diagram = placeDiagrams $ map (declIconDiagram getCodeFragment colorStyle selectedComments) chunks 

diagramSeparation :: Fractional p => p
diagramSeparation = 1.0

placeDiagrams :: SpecialBackend b Double
  => [SpecialQDiagram b Double]
  -> SpecialQDiagram b Double
placeDiagrams diagrams = finalDia where
  finalDia = Dia.vsep diagramSeparation diagrams

declIconDiagram :: SpecialBackend b Double 
  => (Exts.SrcSpan -> String) 
  -> ColorStyle Double 
  -> [Exts.Comment] 
  -> [(Exts.SrcSpan, SyntaxGraph)] 
  -> SpecialQDiagram b Double
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
declDiagram :: SpecialBackend b Double => ColorStyle Double -> DeclQueryValue -> IO (SpecialQDiagram b Double)
declDiagram colorStyle declQV@(DeclQueryValue _ declGraph _ _) = do
  let 
    (topComments, bottomComments) = viewDiagramBase colorStyle declQV
    collapsedDeclGraph = syntaxGraphToCollapsedGraph declGraph
  declDia <- renderIngSyntaxGraph colorStyle (collapsedDeclGraph, collapsedDeclGraph)
  let diagram = topComments Dia.=== Dia.alignL declDia Dia.=== bottomComments
  pure diagram

viewDiagramBase :: SpecialBackend b Double => ColorStyle Double -> DeclQueryValue -> (SpecialQDiagram b Double, SpecialQDiagram b Double)
viewDiagramBase colorStyle (DeclQueryValue _ _ commentsBefore commentsAfter) = (topComments, bottomComments) where
  topComments = placeDiagrams $ map (commentToDiagram colorStyle) commentsBefore
  bottomComments = placeDiagrams $ map (commentToDiagram colorStyle) commentsAfter

nodeDiagram :: SpecialBackend b Double  => ColorStyle Double -> DeclQueryValue -> NodeQueryValue -> IO (SpecialQDiagram b Double)
nodeDiagram colorStyle declQV@(DeclQueryValue _ declGraph _ _) (NodeQueryValue _ name) = do
  let
    collapsedDeclGraph = syntaxGraphToLessCollapsedGraph declGraph
    (topComments, bottomComments) = viewDiagramBase colorStyle declQV
    viewGraph = neighborsSubgraph name collapsedDeclGraph
  nodeDia <- renderIngSyntaxGraph colorStyle (collapsedDeclGraph, viewGraph)
  let diagram = topComments Dia.=== Dia.alignL nodeDia Dia.=== bottomComments
  pure diagram

commentToDiagram :: SpecialBackend b Double
  => ColorStyle Double
  -> Exts.Comment
  -> SpecialQDiagram b Double
commentToDiagram colorStyle (Exts.Comment _ _ c) = Dia.value mempty $ multilineComment colorStyle c

-- Not queryable diagram
staticDiagramFromModule :: SpecialBackend b Double =>
  Bool -> ModuleGraphs -> ColorStyle Double -> IO(SpecialQDiagram b Double)
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
  --print comments
  pure moduleDiagram

composeDiagrams :: SpecialBackend b Double
  => [(Exts.SrcSpan, SpecialQDiagram b Double)]
  -> SpecialQDiagram b Double
composeDiagrams diagrams = finalDiagram where
  sortedDiagarms = snd <$> sortBy (compare `on` fst) diagrams
  finalDiagram = placeDiagrams sortedDiagarms

commentToSpanAndDiagram :: SpecialBackend b Double
  => ColorStyle Double
  -> Exts.Comment
  -> (Exts.SrcSpan, SpecialQDiagram b Double)
commentToSpanAndDiagram colorStyle comment@(Exts.Comment _ srcSpan _) = (srcSpan, commentToDiagram colorStyle comment)

moduleGraphsToViewGraphs :: AnnotatedFGR -> (AnnotatedFGR, AnnotatedFGR)
moduleGraphsToViewGraphs graph = (graph, graph) 

addSourceCodeDiagram :: SpecialBackend b Double
  => ColorStyle Double
  -> SpecialQDiagram b Double
  -> SourceCode
  -> SpecialQDiagram b Double
addSourceCodeDiagram colorStyle diagram codeString = diagramWithsourceCode where
  sourceCodeDia = Dia.value mempty $ sourceCodeDiagram  codeString colorStyle
  diagramWithsourceCode = diagram Dia.=== sourceCodeDia