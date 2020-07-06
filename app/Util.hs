{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Util (
  printSelf
  , iconToPort
  , makeSimpleEdge
  , makeNotConstraintEdge
  , makeInvisibleEdge
  , nameAndPort
  , justName
  , fromMaybeError
  , maybeBoolToBool
  , nodeNameToInt
  , customRenderSVG
  , namedToTuple
  , tupleToNamed
  , queryValue
  , showSrcInfo
  ) where

import Diagrams.Backend.SVG(renderSVG', Options(..), SVG)
import qualified Language.Haskell.Exts.SrcLoc as SrcLoc
import qualified Diagrams.Prelude as Dia
import Graphics.Svg.Attributes(bindAttr, AttrTag(..))
import Data.Maybe(fromMaybe)
import Data.Text as T(pack, filter, Text) 
import Data.Char(isAlpha)
import System.FilePath(takeBaseName)
import qualified Data.Set as Set
import Data.Typeable(Typeable)
import qualified Debug.Trace

import           Types (
  Edge(..)
  , Icon(..)
  , NamedIcon(..)
  , NameAndPort(..)
  , Connection
  , NodeName(..)
  , Port
  , Named(..)
  , EdgeOption(..)
  , DiaQuery(..)
  , SrcRef
  )


makeSimpleEdge :: Connection -> Edge
makeSimpleEdge = Edge DrawAndConstraint

makeNotConstraintEdge :: Connection -> Edge
makeNotConstraintEdge = Edge DrawAndNotConstraint

makeInvisibleEdge :: Connection -> Edge
makeInvisibleEdge = Edge DoNotDrawButConstraint

nameAndPort :: NodeName -> Port -> NameAndPort
nameAndPort n p = NameAndPort n (Just p)

justName :: NodeName -> NameAndPort
justName n = NameAndPort n Nothing

-- BEGIN Edge constructors --
iconToPort :: NodeName -> NodeName -> Port -> Edge
iconToPort a   c d = makeSimpleEdge (justName a, nameAndPort c d)

-- END Edge constructors --

fromMaybeError :: String -> Maybe a -> a
fromMaybeError s = fromMaybe (error s)

printSelf :: (Show a) => a -> a
printSelf a = Debug.Trace.trace (show a ++ "\n\n") a

-- | (Just True) = True, Nothing = False
maybeBoolToBool :: Maybe Bool -> Bool
maybeBoolToBool = or

nodeNameToInt :: NodeName -> Int
nodeNameToInt (NodeName x) = x

namedToTuple :: Named a -> (NodeName, a)
namedToTuple (Named x y) = (x, y)

tupleToNamed :: (NodeName, a) -> Named a
tupleToNamed (x, y) = Named x y

customRenderSVG :: (Typeable n, Show n, RealFloat n) =>
  FilePath
  -> Dia.SizeSpec Dia.V2 n
  -> Dia.QDiagram SVG Dia.V2 n Dia.Any
  -> IO ()
customRenderSVG outputFilename size = renderSVG' outputFilename svgOptions where
  -- This xml:space attribute preserves the whitespace in the svg text.
  attributes = [bindAttr XmlSpace_ (pack "preserve")]
  -- https://github.com/diagrams/diagrams-svg/blob/master/src/Diagrams/Backend/SVG.hs#L367
  mkPrefix :: FilePath -> T.Text
  mkPrefix = T.filter isAlpha . T.pack . takeBaseName

  svgOptions = SVGOptions size Nothing (mkPrefix outputFilename) attributes True

queryValue :: (NamedIcon -> DiaQuery)
queryValue (Named _ (Icon _ srcRef)) = [srcRef]

showSrcInfo :: DiaQuery -> String
showSrcInfo (x:_) = SrcLoc.srcSpanFilename x 
  ++ ":" ++ (show $ SrcLoc.srcSpanStartLine x)
  ++ ":" ++ (show $ SrcLoc.srcSpanStartColumn x)
showSrcInfo [] = error "Nothing is clicked"