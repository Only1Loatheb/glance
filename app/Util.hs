{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Util (
  printSelf
  , printSelfM
  , makeSimpleEdge
  , makeNotConstraintEdge
  , makeInvisibleEdge
  , makeImportantEdge
  , nameAndPort
  , fromMaybeError
  , maybeBoolToBool
  , nodeNameToInt
  , namedToTuple
  , tupleToNamed
  , showSrcInfo
  , hasSrcRef
  , getSrcRef
  , fmapMaybeM
  ) where

import qualified Language.Haskell.Exts.SrcLoc as SrcLoc
import Data.Maybe(fromMaybe)
import qualified Debug.Trace

import           Types (
  Edge(..)
  , NameAndPort(..)
  , Connection
  , NodeName(..)
  , Port
  , Named(..)
  , EdgeOption(..)
  , NodeQueryValue(..)
  , DeclQueryValue(..)
  , SrcRef
  , View
  )


makeSimpleEdge :: Connection -> Edge
makeSimpleEdge = Edge DrawAndConstraint

makeNotConstraintEdge :: Connection -> Edge
makeNotConstraintEdge = Edge DrawAndNotConstraint

makeImportantEdge :: Connection -> Edge
makeImportantEdge = Edge DrawAsImportant

makeInvisibleEdge :: Int -> Connection -> Edge
makeInvisibleEdge len = Edge (DoNotDrawButConstraint len)

nameAndPort :: NodeName -> Port -> NameAndPort
nameAndPort n p = Named n p

-- END Edge constructors --

fromMaybeError :: String -> Maybe a -> a
fromMaybeError s = fromMaybe (error s)

printSelf :: (Show a) => a -> a
printSelf a = Debug.Trace.trace ("\nPrinted\n" ++ show a ++ "\n") a

printSelfM :: (Applicative f, Show a) => a -> f ()
printSelfM a = Debug.Trace.traceM ("\nPrinted\n" ++ show a ++ "\n")

-- | (Just True) = True, Nothing = False
maybeBoolToBool :: Maybe Bool -> Bool
maybeBoolToBool = or

nodeNameToInt :: NodeName -> Int
nodeNameToInt (NodeName x) = x

namedToTuple :: Named a -> (NodeName, a)
namedToTuple (Named x y) = (x, y)

tupleToNamed :: (NodeName, a) -> Named a
tupleToNamed (x, y) = Named x y

showSrcInfo :: SrcRef -> String
showSrcInfo srcRef = SrcLoc.srcSpanFilename srcRef
  ++ ":" ++ show (SrcLoc.srcSpanStartLine srcRef)
  ++ ":" ++ show (SrcLoc.srcSpanStartColumn srcRef)

hasSrcRef :: View -> Bool
hasSrcRef (Nothing, Nothing) = False
hasSrcRef _ = True

getSrcRef :: View -> SrcRef
getSrcRef (_, Just nodeQV) = srcRef where
  srcRef = srcRefNQV nodeQV
getSrcRef (Just declQV, _) = srcRef where
  srcRef = srcRefDQV declQV
getSrcRef _ = error "No source in View"

fmapMaybeM :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
fmapMaybeM _ Nothing  = return Nothing
fmapMaybeM f (Just x) = f x >>= (return . Just)