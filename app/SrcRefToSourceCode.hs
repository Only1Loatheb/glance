
module SrcRefToSourceCode (srcRefToSourceCode) where

import qualified Language.Haskell.Exts as Exts

import Types (SrcRef, SourceCode)
import Data.List.Split as Split

srcRefToSourceCode :: String -> SrcRef -> SourceCode
srcRefToSourceCode source srcRef = fragment where
  sourceLines = lines source
  placesToSplit = [Exts.srcSpanStartLine srcRef - 1,Exts.srcSpanEndLine srcRef - Exts.srcSpanStartLine srcRef + 1]
  referencedLines = flip (!!) 1 $ Split.splitPlacesBlanks placesToSplit sourceLines
  fragment = unlines referencedLines

