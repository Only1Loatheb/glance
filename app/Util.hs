{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Util (
  printSelf
  , makeSimpleEdge
  , makeNotConstraintEdge
  , makeInvisibleEdge
  , nameAndPort
  , fromMaybeError
  , maybeBoolToBool
  , nodeNameToInt
  , namedToTuple
  , tupleToNamed
  , queryValue
  , showSrcInfo
  ) where

import qualified Language.Haskell.Exts.SrcLoc as SrcLoc
import Data.Maybe(fromMaybe)
import qualified Data.Set as Set
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
  , DiaQuery
  , QueryValue(..)
  , SrcRef
  )


makeSimpleEdge :: Connection -> Edge
makeSimpleEdge = Edge DrawAndConstraint

makeNotConstraintEdge :: Connection -> Edge
makeNotConstraintEdge = Edge DrawAndNotConstraint

makeInvisibleEdge :: Connection -> Edge
makeInvisibleEdge = Edge DoNotDrawButConstraint

nameAndPort :: NodeName -> Port -> NameAndPort
nameAndPort n p = NameAndPort n p

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

queryValue :: (NamedIcon -> DiaQuery)
queryValue (Named name (Icon _ srcRef)) = [QueryValue srcRef name]

showSrcInfo :: QueryValue -> String
showSrcInfo q = SrcLoc.srcSpanFilename srcRef
  ++ ":" ++ show (SrcLoc.srcSpanStartLine srcRef)
  ++ ":" ++ show (SrcLoc.srcSpanStartColumn srcRef) where
    srcRef = nodeSrcRef q