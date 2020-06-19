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
import qualified Data.Set as Set
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )

import qualified Language.Haskell.Exts as Exts
import           Data.List( sortBy )
import           Data.Function( on )

import IconToSymbolDiagram(ColorStyle(..), colorScheme, multilineComment)
import Rendering(renderIngSyntaxGraph)
import CollapseGraph(translateModuleToCollapsedGraphs)
import           Types  ( SpecialQDiagram
                        , SpecialBackend
                        , NamedIcon
                        )

diagramFromModule :: SpecialBackend b Double =>
  String -> Bool -> IO (SpecialQDiagram b Double, Dia.P2 Double -> Maybe NamedIcon)
diagramFromModule inputFilename includeComments = do
  parseResult <- parseModule inputFilename
  let
    (parsedModule, comments) = Exts.fromParseResult parseResult
    drawingsGraphs = translateModuleToCollapsedGraphs parsedModule
    declarationSpans = moduleToSrcSpanStarts parsedModule
  --print parsedModule
  --print "\n\n"
  --print drawings

  declarationDiagramsAndPointToIcon <- traverse (renderIngSyntaxGraph "") drawingsGraphs
  let
    commentDiagramsAndNothing = fmap commentAndPointToIcon comments
    declarations = zip declarationSpans declarationDiagramsAndPointToIcon

    diagramsAndIconPosition = if includeComments then commentDiagramsAndNothing ++ declarations else declarations
    moduleDiagramAndPointToIcon = composeDiagrams diagramsAndIconPosition
  --print comments
  pure moduleDiagramAndPointToIcon

commentAndPointToIcon :: SpecialBackend b Double
  => Exts.Comment
  -> (Exts.SrcSpan, (SpecialQDiagram b Double, Dia.P2 Double -> Maybe NamedIcon))
commentAndPointToIcon (Exts.Comment _ srcSpan c) = (srcSpan, (multilineComment  c, const Nothing))

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

composeDiagrams :: SpecialBackend b Double
  =>  [(Exts.SrcSpan, (SpecialQDiagram b Double, Dia.P2 Double -> Maybe NamedIcon))]
  ->   (SpecialQDiagram b Double, Dia.P2 Double -> Maybe NamedIcon)
composeDiagrams diagrams = finalDiagram where
  sortedDiagarms = snd <$> sortBy (compare `on` fst) diagrams
  finalDiagram = composeDiagramsInModule sortedDiagarms

composeDiagramsInModule :: SpecialBackend b Double
  => [(SpecialQDiagram b Double, Dia.P2 Double -> Maybe NamedIcon)]
  -> (SpecialQDiagram b Double, Dia.P2 Double -> Maybe NamedIcon)
composeDiagramsInModule diagramAndPointToIcons = (finalDia, pointToIcon) where
  (diagrams, pointToIcons) = unzip diagramAndPointToIcons
  moduleDiagram = Dia.vcat diagrams
  finalDia = Dia.bg (backgroundC colorScheme) moduleDiagram

  diagramHeights = map Dia.height diagrams
  pointToIcon = composePointToIcons diagramHeights pointToIcons

composePointToIcons ::
  [Double]
  -> [Dia.P2 Double -> Maybe NamedIcon]
  -> Dia.P2 Double
  -> Maybe NamedIcon
composePointToIcons diagramHeights pointToIcons point@(Dia.P (Dia.V2 _x y)) = maybeIcon where
  diagramsTopY = scanl (+) 0.0 diagramHeights
  maxYToPointToIcon = Map.fromList $ zip diagramsTopY pointToIcons
  maybePointToIcon = listToMaybe $ Map.toDescList $ fst $ Map.split y maxYToPointToIcon
  (diagramTopY, pointToIcon) = fromMaybe (0.0, const Nothing) maybePointToIcon
  adjustedPoint = point Dia.^-^ Dia.unitY Dia.^* diagramTopY
  maybeIcon = pointToIcon adjustedPoint
    -- $ error $ "topY: " ++ show diagramTopY ++ " adjustedPoint: " ++ show adjustedPoint

