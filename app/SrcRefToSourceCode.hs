
module SrcRefToSourceCode (srcRefToSourceCode) where

import System.IO 
import qualified Language.Haskell.Exts as Exts

import Types (SrcRef)
import Data.List.Split as Split

srcRefToSourceCode source srcRef = fragment where
  sourceLines = lines source
  placesToSplit = [Exts.srcSpanStartLine srcRef - 1,Exts.srcSpanEndLine srcRef - Exts.srcSpanStartLine srcRef + 1]
  referencedLines = flip (!!) 1 $ Split.splitPlacesBlanks placesToSplit sourceLines
  fragment = unlines referencedLines

