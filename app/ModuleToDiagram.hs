{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module ModuleToDiagram(
  diagramFromModule
  ) where
-- import Prelude hiding (return)

-- Note: (#) and (&) are hidden in all Glance source files, since they would
-- require a special case when translating when Glance is run on its own source
-- code.
import qualified Diagrams.Prelude as Dia hiding ((#), (&))
import qualified Data.Map as Map
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )

import qualified Language.Haskell.Exts as Exts
import           Data.List( sortBy )
import           Data.Function( on )

import IconToSymbolDiagram(ColorStyle(..), colorScheme, multilineComment)
import Rendering(renderIngSyntaxGraph)
import CollapseGraph(translateModuleToCollapsedGraphs)
import           Types  ( 
  SpecialDiagram  
  , SpecialQDiagram
  , SpecialBackend
  , NamedIcon
  )

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

diagramFromModule :: SpecialBackend b Double =>
  String -> Bool -> IO (SpecialQDiagram b Double)
diagramFromModule inputFilename includeComments = do
  parseResult <- parseModule inputFilename
  let
    (parsedModule, comments) = Exts.fromParseResult parseResult
    drawingsGraphs = translateModuleToCollapsedGraphs parsedModule
  --print drawingsGraphs
  declarationDiagrams <- traverse (renderIngSyntaxGraph "") drawingsGraphs
  let
    declarationSpans = moduleToSrcSpanStarts parsedModule
    -- spanAndDeclarations :: [(Exts.SrcSpan, SpecialQDiagram b Double)]
    spanAndDeclarations = zip declarationSpans declarationDiagrams

    -- spanAndcomments :: [(Exts.SrcSpan, SpecialQDiagram b Double)]
    spanAndcomments = fmap commentToDiagram comments

    spanAndDiagrams = if includeComments 
      then spanAndcomments ++ spanAndDeclarations 
      else spanAndDeclarations
    moduleDiagram = composeDiagrams spanAndDiagrams
  --print comments
  pure moduleDiagram

commentToDiagram :: SpecialBackend b Double
  => Exts.Comment
  -> (Exts.SrcSpan, SpecialQDiagram b Double)
commentToDiagram (Exts.Comment _ srcSpan c) = (srcSpan, Dia.value mempty $ multilineComment  c)

composeDiagrams :: SpecialBackend b Double
  =>  [(Exts.SrcSpan, SpecialQDiagram b Double)]
  ->   (SpecialQDiagram b Double)
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