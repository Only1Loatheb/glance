{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module ModuleToDiagram(
  diagramFromModule
  ) where
-- import Prelude hiding (return)

-- Note: (#) and (&) are hidden in all Glance source files, since they would
-- require a special case when translating when Glance is run on its own source
-- code.
import qualified Diagrams.Prelude as Dia hiding ((#), (&))

import qualified Language.Haskell.Exts as Exts
import           Data.List( sortBy )
import           Data.Function( on )

import IconToSymbolDiagram(ColorStyle(..), colorScheme, multilineComment)
import Rendering(renderIngSyntaxGraph)
import CollapseGraph(translateModuleToCollapsedGraphs)
import           Types  ( SpecialQDiagram
                        , SpecialBackend
                        )

diagramFromModule :: SpecialBackend b Double =>
  String -> Bool -> IO (SpecialQDiagram b Double)
diagramFromModule inputFilename includeComments = do
  parseResult <- parseModule inputFilename
  let
    (parsedModule, comments) = Exts.fromParseResult parseResult
    drawingsGraphs = translateModuleToCollapsedGraphs parsedModule
    declarationSpans = moduleToSrcSpanStarts parsedModule
  --print parsedModule
  --print "\n\n"
  --print drawings

  declarationDiagrams <- traverse (renderIngSyntaxGraph "") drawingsGraphs
  let
    commentsInBoxes
      = fmap (\(Exts.Comment _ srcSpan c) -> (srcSpan, multilineComment  c) ) comments

    declarations = zip declarationSpans declarationDiagrams
    
    diagrams = if includeComments then commentsInBoxes ++ declarations else declarations
    moduleDiagram = composeDiagrams diagrams
  --print comments
  return moduleDiagram
  
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

composeDiagrams :: SpecialBackend b n 
  =>  [(Exts.SrcSpan, SpecialQDiagram b n)]->   SpecialQDiagram b n
composeDiagrams diagrams = finalDiagram where
  sortedDiagarms = snd <$> sortBy (compare `on` fst) diagrams
  finalDiagram = composeDiagramsInModule sortedDiagarms

composeDiagramsInModule :: SpecialBackend b n 
  => [SpecialQDiagram b n]->   SpecialQDiagram b n
composeDiagramsInModule diagrams = finalDia where
  moduleDiagram = Dia.vsep 1 $ fmap Dia.alignL diagrams
  finalDia = Dia.bgFrame 1 (backgroundC colorScheme) moduleDiagram