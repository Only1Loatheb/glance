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

import           Types  (
  SpecialDiagram
  , SpecialQDiagram
  , SpecialBackend
  , NamedIcon
  , SrcRef
  , NodeQueryValue(..)
  , ModuleGraphs
  , ViewGraphs
  )
import SimpSyntaxToSyntaxGraph(translateDeclToSyntaxGraph)
import HsSyntaxToSimpSyntax(hsDeclToSimpDecl)

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

moduleToDecls ::
  Exts.Module Exts.SrcSpanInfo -> [Exts.Decl Exts.SrcSpanInfo]
moduleToDecls (Exts.Module _ _ _ _ decls) = decls
moduleToDecls moduleSyntax = error $ "Unsupported syntax in moduleToDecls: "
    <> show moduleSyntax

getSrcSpans = map (Exts.srcInfoSpan . Exts.ann)

getModuleGraphs :: String -> IO(ModuleGraphs)
getModuleGraphs inputFilename = do
  parseResult <- parseModule inputFilename
  let
    (parsedModule, comments) = Exts.fromParseResult parseResult
    decls = moduleToDecls parsedModule
    declGraphs = map (translateDeclToSyntaxGraph . hsDeclToSimpDecl) decls
    declSpans = getSrcSpans decls
    declSpansAndGraphs = zip declSpans declGraphs
  pure (declSpansAndGraphs, comments)