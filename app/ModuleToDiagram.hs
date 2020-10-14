{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module ModuleToDiagram(
  diagramFromModule
  , getModuleGraphs
  , selectView
  , staticDiagramFromModule
  , moduleGraphsToViewGraphs
  -- for tests
  , selectGraph
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
  , QueryValue(..)
  , ModuleGraphs
  , ViewGraphs
  )

import PartialView (neighborsSubgraph)

-- moduleGraphs

parseModule :: String
  -> IO (Exts.ParseResult (Exts.Module Exts.SrcSpanInfo, [Exts.Comment]))
parseModule inputFilename =
  Exts.parseFileWithComments
    (Exts.defaultParseMode {
        Exts.extensions = [Exts.EnableExtension Exts.MultiParamTypeClasses
                          , Exts.EnableExtension Exts.FlexibleContexts]
        , Exts.parseFilename = inputFilename
        })
    inputFilename

moduleToSrcSpanStarts ::
  Exts.Module Exts.SrcSpanInfo -> [Exts.SrcSpan]
moduleToSrcSpanStarts (Exts.Module _ _ _ _ decls)
  = fmap (Exts.srcInfoSpan . Exts.ann) decls
moduleToSrcSpanStarts moduleSyntax
  = error $ "Unsupported syntax in moduleToSrcSpanStarts: "
    <> show moduleSyntax

getModuleGraphs :: String -> IO(ModuleGraphs)
getModuleGraphs inputFilename = do
  parseResult <- parseModule inputFilename
  let
    (parsedModule, comments) = Exts.fromParseResult parseResult
    declGraphs = translateModuleToCollapsedGraphs parsedModule
    declSpans = moduleToSrcSpanStarts parsedModule
    declSpansAndGraphs = zip declSpans declGraphs
  pure (declSpansAndGraphs, comments)

diagramFromModule :: SpecialBackend b Double =>
  Bool -> ViewGraphs -> Maybe String -> IO(SpecialQDiagram b Double)
diagramFromModule includeComments (declSpansAndGraphs, comments) maybeCodeString = do
  let (declarationSpans, drawingsGraphs) = unzip declSpansAndGraphs
  --print drawingsGraphs
  declarationDiagrams <- traverse ( renderIngSyntaxGraph "" ) drawingsGraphs
  let
    spanAndDeclarations = zip declarationSpans declarationDiagrams

    spanAndcomments = fmap commentToDiagram comments

    spanAndDiagrams = if includeComments
      then spanAndcomments ++ spanAndDeclarations
      else spanAndDeclarations
    moduleDiagram = composeDiagrams spanAndDiagrams
  --print comments
  addSourceCodeDiagram moduleDiagram maybeCodeString

staticDiagramFromModule :: SpecialBackend b Double =>
  Bool -> ModuleGraphs -> IO(SpecialQDiagram b Double)
staticDiagramFromModule includeComments moduleGraphs 
  = diagramFromModule includeComments (moduleGraphsToViewGraphs moduleGraphs) Nothing

addSourceCodeDiagram moduleDiagram maybeCodeString = do
  case maybeCodeString of
    (Just codeString) -> do
      let sourceCodeDia = Dia.value mempty $ sourceCodeDiagram codeString
      pure (moduleDiagram Dia.===  sourceCodeDia)
    _ -> do
      pure (moduleDiagram)

commentToDiagram :: SpecialBackend b Double
  => Exts.Comment
  -> (Exts.SrcSpan, SpecialQDiagram b Double)
commentToDiagram (Exts.Comment _ srcSpan c) = (srcSpan, Dia.value mempty $ multilineComment  c)

composeDiagrams :: SpecialBackend b Double
  => [(Exts.SrcSpan, SpecialQDiagram b Double)]
  -> (SpecialQDiagram b Double)
composeDiagrams diagrams = finalDiagram where
  sortedDiagarms = snd <$> sortBy (compare `on` fst) diagrams
  finalDiagram = composeDiagramsInModule sortedDiagarms

diagramSeparation :: Fractional p => p
diagramSeparation = 1.0

composeDiagramsInModule :: SpecialBackend b Double
  => [SpecialQDiagram b Double]
  -> SpecialQDiagram b Double
composeDiagramsInModule diagrams = finalDia where
  finalDia = Dia.vsep diagramSeparation diagrams

selectView :: QueryValue -> ModuleGraphs -> ViewGraphs
selectView (QueryValue srcRef name) (declSpansAndGraphs, comments) = ([selectedSpanAndGraph], nerbyComments) where
  ((declSpan, graph), srcSpans) = selectGraph srcRef declSpansAndGraphs
  nerbyComments = selectComments comments srcSpans
  selectedSpanAndGraph = (declSpan, (graph, neighborsSubgraph name graph))

selectGraph :: Exts.SrcSpan
  -> [(Exts.SrcSpan, b)]
  -> ((Exts.SrcSpan, b), (Exts.SrcSpan, Exts.SrcSpan))
selectGraph srcRef declSpansAndGraphs = (declSpanAndGraph, (srcSpanBefore, srcSpanAfter)) where
  declSpansAndGraphsChunks = Split.divvy chunkSize chunkOffset paddedDeclSpansAndGraphs where
    chunkSize = 3
    chunkOffset = 1
    paddedDeclSpansAndGraphs = headPadding : declSpansAndGraphs ++ [tailPadding] where
      (headPadding, tailPadding) = getPadding declSpansAndGraphs
  srcRefStart = Exts.srcSpanStart srcRef
  selectedChunk = head $ filter (filterF . Exts.srcSpanStart . fst . flip (!!) 2) declSpansAndGraphsChunks
  filterF = \ x -> x > srcRefStart
  [(srcSpanBefore,_), declSpanAndGraph, (srcSpanAfter,_)] = selectedChunk

getPadding :: [(Exts.SrcSpan, b1)]
                -> ((Exts.SrcSpan, b2), (Exts.SrcSpan, b2))
getPadding declSpansAndGraphs = (headPadding, tailPadding) where
  fileName = Exts.srcSpanFilename $ fst $ head declSpansAndGraphs
  paddingError = error "padding used as a graph"
  headPadding = (srcRefBefere, paddingError) where
    srcRefBefere = Exts.SrcSpan fileName 0 0 0 0
  tailPadding = (srcRefAfter, paddingError) where
    srcRefAfter = Exts.SrcSpan fileName maxBound maxBound maxBound maxBound

selectComments :: [Exts.Comment]
                    -> (Exts.SrcSpan, Exts.SrcSpan) -> [Exts.Comment]
selectComments comments (srcSpanBefore, srcSpanAfter) = nerbyComments where
  nerbyComments = filter (\((Exts.Comment _ srcSpan _))-> srcSpan > srcSpanBefore && srcSpan < srcSpanAfter) comments

moduleGraphsToViewGraphs :: ModuleGraphs -> ViewGraphs
moduleGraphsToViewGraphs (moduleGraphs, comments) = (map moduleGraphToView moduleGraphs, comments) where
  moduleGraphToView (srcRef,graph) = (srcRef,(graph, graph)) 