{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NodeRecordLabels(
  recordLabels
  , showNamedPortRrecord
  ) where

import Diagrams.Prelude hiding ((&), (#), Name)
import qualified Data.GraphViz.Attributes.Complete as GVA
import qualified Data.Text.Lazy as T
import Data.List (transpose)

import Types  (
  Named(..)
  , Icon(..)
  , DiagramIcon(..) 
  , NodeName(..)
  , IconInfo
  , laValue
  , NameAndPort
  )                         
import Icons(findMaybeIconFromName)

import PortConstants(
  argPortList
  , valuePortList
  , pattern InputPort
  , pattern ResultPort
  , listCompQualPortList
  , pattern PatternUnpackingPort
  )

showNamedPortRrecord :: NameAndPort -> String -- should not use ":" because of GraphViz limitations
showNamedPortRrecord (Named nodeName port) = show (toName nodeName) ++ "," ++ show (toName port)

recordPort :: NameAndPort -> GVA.RecordField
recordPort = GVA.PortName . GVA.PN . T.pack . showNamedPortRrecord

emptyRecord :: GVA.RecordField
emptyRecord = GVA.FieldLabel $ T.pack "empty"

inputRecord :: NodeName -> GVA.RecordField
inputRecord name = recordPort $ Named name InputPort

resultRecord :: NodeName -> GVA.RecordField
resultRecord name = recordPort $ Named name ResultPort

valueRecords :: NodeName -> [GVA.RecordField]
valueRecords name = map (recordPort . Named name) valuePortList

argRecords :: NodeName -> [GVA.RecordField]
argRecords name = map (recordPort . Named name) argPortList

qualRecords :: NodeName -> [GVA.RecordField]
qualRecords name = map (recordPort . Named name) listCompQualPortList

patternUnpackingRecord :: NodeName -> GVA.RecordField
patternUnpackingRecord name = recordPort $ Named name PatternUnpackingPort

nestedRecordLabels ::
  IconInfo
  -> GVA.RecordField
  -> Maybe NodeName
  -> GVA.RecordFields
nestedRecordLabels iconInfo defaultRecord maybeNodeName = records where
  records = case findMaybeIconFromName iconInfo maybeNodeName of
      Nothing -> [defaultRecord]
      Just (Named name icon) -> recordLabels iconInfo icon name

recordLabels :: IconInfo -> Icon -> NodeName -> GVA.RecordFields -- this mirrors IconToDiagram layouts
recordLabels iconInfo (Icon icon _) name = case icon of
  TextBoxIcon {} -> [patternUnpackingRecord name, resultRecord name]
  FunctionArgIcon argLabels _->  take (length argLabels) (valueRecords name)
  FunctionDefIcon _ mBodyNode -> records where
    records = [GVA.FlipFields $ bodyRecord ++ [resultRecord name]]
    bodyRecord = nestedRecordLabels iconInfo (inputRecord name) mBodyNode
  BindTextBoxIcon {} -> [inputRecord name]
  NestedApply _ fun argLabels -> records where
    records = [GVA.FlipFields $ args : funRecord ++ [resultRecord name]]
    args = GVA.FlipFields $ mconcat $ zipWith (nestedRecordLabels iconInfo) (argRecords name) argLabels
    funRecord = nestedRecordLabels iconInfo (inputRecord name) fun
  NestedPatternApp _ argLabels rhsNodeName -> [GVA.FlipFields [topRecords, bottomRecords]] where
    topRecords = GVA.FlipFields $ inputRecord name : emptyRecord : args
    argLenght = length argLabels
    bottomRecords = GVA.FlipFields $ emptyRecord : patRecord ++ replicate argLenght emptyRecord
    patternRecord = patternUnpackingRecord name
    patRecord = nestedRecordLabels iconInfo patternRecord rhsNodeName
    args = mconcat $ zipWith (nestedRecordLabels iconInfo) (valueRecords name) (toListOf (traverse . laValue) argLabels )
  NestedCaseIcon _ arg condsAndVals ->  leftSide : records  where
    leftSide = GVA.FlipFields $ args ++ [resultRecord name]
    (conds,vals) = unzip condsAndVals
    condRecords = zipWith (nestedRecordLabels iconInfo) (argRecords name) conds  
    valRecords  = zipWith (nestedRecordLabels iconInfo) (valueRecords name) vals
    records = concat . concat . transpose $ [condRecords, valRecords]
    args = nestedRecordLabels iconInfo (inputRecord name) arg
  CaseResultIcon -> [GVA.FlipFields [inputRecord name, resultRecord name]]
  ListCompIcon item gens quals -> [main, rightSide] where
    main = GVA.FlipFields $ generators : emptyRecord : itemRecord ++ [resultRecord name]
    generators = GVA.FlipFields $ concat $ zipWith (nestedRecordLabels iconInfo) (argRecords name) gens 
    rightSide = GVA.FlipFields [emptyRecord, qualifiers ,emptyRecord]
    qualifiers = GVA.FlipFields $ concat $ zipWith (nestedRecordLabels iconInfo) (qualRecords name) quals
    itemRecord = nestedRecordLabels iconInfo (inputRecord name) item
  ListLitIcon _ argNodes _ -> records where
    records = [GVA.FlipFields [patternUnpackingRecord name, args, resultRecord name]]
    args = GVA.FlipFields $ mconcat $ zipWith (nestedRecordLabels iconInfo) (argRecords name) argNodes
