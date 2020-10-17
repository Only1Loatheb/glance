{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module ModuleToGraphs(
  getModuleGraphs
  ) where
-- import Prelude hiding (return)

-- Note: (#) and (&) are hidden in all Glance source files, since they would
-- require a special case when translating when Glance is run on its own source
-- code.
import qualified Diagrams.Prelude as Dia hiding ((#), (&))
import           Data.Maybe
import qualified Language.Haskell.Exts as Exts

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

getModuleGraphs :: String -> IO(ModuleGraphs)
getModuleGraphs inputFilename = do
  parseResult <- parseModule inputFilename
  let
    (parsedModule, comments) = Exts.fromParseResult parseResult
    declGraphs = translateModuleToCollapsedGraphs parsedModule
    declSpans = moduleToSrcSpanStarts parsedModule
    declSpansAndGraphs = zip declSpans declGraphs
  pure (declSpansAndGraphs, comments)