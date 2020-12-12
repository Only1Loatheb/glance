{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NodeRecordLabels(
  recordLabels
  ) where

import Diagrams.Prelude hiding ((&), (#), Name)
import qualified Data.GraphViz.Attributes.Complete as GVA
import qualified Data.Text.Lazy as T
import Data.List (transpose)

import Types  (
  Named(..)
  , NumericType
  , Icon(..)
  , DiagramIcon(..) 
  , NodeName(..)
  , Port(..)
  , NamedIcon
  , Labeled(..)
  , IconInfo
  , laValue
  )                         
import Icons(findIcon,findMaybeIconFromName,findMaybeIconsFromNames)

import PortConstants(
  argPortsConst
  , resultPortsConst
  , pattern InputPortConst
  , pattern ResultPortConst
  , isArgPort
  , isQualPort
  , listCompQualPorts
  , caseValuePorts
  , caseConditionPorts
  , pattern PatternUnpackingPort
  )
import Data.Maybe (isNothing, isJust)

recordPort :: IsName a => a -> GVA.RecordField
recordPort = GVA.PortName . GVA.PN . T.pack . show . toName

emptyRecord :: GVA.RecordField
emptyRecord = GVA.FieldLabel $ T.pack "empty"

inputRecord :: GVA.RecordField
inputRecord = recordPort InputPortConst

resultRecord :: GVA.RecordField
resultRecord = recordPort ResultPortConst

valueRecords :: [GVA.RecordField]
valueRecords = map recordPort resultPortsConst

argRecords :: [GVA.RecordField]
argRecords = map recordPort argPortsConst

recordLabels :: IconInfo -> Icon -> Maybe NodeName -> GVA.RecordFields
recordLabels = getRecordLabels

nestedRecordLabels ::
  IconInfo
  -> GVA.RecordField
  -> Maybe NodeName
  -> GVA.RecordFields
nestedRecordLabels iconInfo defaultRecord maybeNodeName = records where
  records = case findMaybeIconFromName iconInfo maybeNodeName of
      Nothing -> [defaultRecord]
      Just (Named _name icon) -> getRecordLabels iconInfo icon Nothing

getRecordLabels :: IconInfo -> Icon -> Maybe NodeName -> GVA.RecordFields
getRecordLabels iconInfo (Icon icon _) maybeNodeName = case icon of
  TextBoxIcon {} -> [resultRecord]
  FunctionArgIcon argLabels ->  take (length argLabels) valueRecords
  FunctionDefIcon _ _ mBodyNode -> records where
    records = [GVA.FlipFields $ bodyRecord ++ [resultRecord]]
    bodyRecord = nestedRecordLabels iconInfo inputRecord mBodyNode
  BindTextBoxIcon {} -> [inputRecord]
  NestedApply _ fun argLabels -> records where
    records = [GVA.FlipFields $ args : funRecord ++ [resultRecord]]
    args = GVA.FlipFields $ mconcat $ zipWith (nestedRecordLabels iconInfo) argRecords argLabels
    funRecord = nestedRecordLabels iconInfo inputRecord fun
  NestedPatternApp constructor argLabels rhsNodeName -> [GVA.FlipFields [topRecords, bottomRecords]] where
    topRecords = GVA.FlipFields $ inputRecord : emptyRecord : args
    args = take argLenght argRecords
    argLenght = length argLabels
    bottomRecords = GVA.FlipFields $ emptyRecord : patRecord ++ replicate argLenght emptyRecord
    patternRecord = recordPort PatternUnpackingPort
    patRecord = nestedRecordLabels iconInfo patternRecord rhsNodeName
    --TODO [constructor, argLabels] are icons instead of Names
  NestedCaseIcon _ arg condsAndVals ->  leftSide : records  where
    leftSide = GVA.FlipFields $ args ++ [resultRecord]
    (conds,vals) = unzip condsAndVals
    condRecords = zipWith (nestedRecordLabels iconInfo) argRecords conds  
    valRecords  = zipWith (nestedRecordLabels iconInfo) valueRecords vals
    records = concat . concat . transpose $ [condRecords, valRecords]
    args = nestedRecordLabels iconInfo inputRecord arg
  CaseResultIcon -> [GVA.FlipFields [inputRecord, resultRecord]]
  ListCompIcon item gens quals -> [main, rightSide] where
    main = GVA.FlipFields $ generators : emptyRecord : itemRecord ++ [resultRecord]
    generators = GVA.FlipFields $ concat $ zipWith (nestedRecordLabels iconInfo) argRecords gens 
    rightSide = GVA.FlipFields [emptyRecord, qualifiers ,emptyRecord]
    qualifiers = GVA.FlipFields $ concat $ zipWith (nestedRecordLabels iconInfo) (map recordPort listCompQualPorts) quals
    itemRecord = nestedRecordLabels iconInfo inputRecord item
  ListLitIcon _ argNodes _ -> records where
    records = [GVA.FlipFields [args, resultRecord]]
    args = GVA.FlipFields $ mconcat $ zipWith (nestedRecordLabels iconInfo) argRecords argNodes
