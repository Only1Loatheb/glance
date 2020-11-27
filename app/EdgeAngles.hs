{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EdgeAngles(
  getPortAngle
  ) where

import Diagrams.Prelude hiding ((&), (#), Name)


import Types  (
  NumericType
  , Icon(..)
  , DiagramIcon(..) 
  , NodeName(..)
  , Port(..)
  , NamedIcon
  , Labeled(..)
  , IconInfo
  )                         
import Icons(findIcon,findMaybeIconFromName,findMaybeIconsFromNames)

import PortConstants(
    pattern InputPortConst
  , pattern ResultPortConst
  , isArgPort
  , isQualPort
  )

applyPortAngle :: Port -> Angle NumericType
applyPortAngle InputPortConst = 1/2 @@ turn -- input function
applyPortAngle ResultPortConst = 3/4 @@ turn
applyPortAngle _isInput = 1/4 @@ turn

nestedApplyPortAngle :: Port -> Angle NumericType
nestedApplyPortAngle InputPortConst = 1/2 @@ turn -- input function
nestedApplyPortAngle ResultPortConst = 3/4 @@ turn
nestedApplyPortAngle _isInput = 3/16 @@ turn

lambdaPortAngle :: Bool -> Port -> Angle NumericType
lambdaPortAngle _ InputPortConst = 1/4 @@ turn
lambdaPortAngle _ ResultPortConst = 3/4 @@ turn
lambdaPortAngle isEmbedded port
  | isArgPort port = if isEmbedded then 1/2 @@ turn else 1/4 @@ turn
  | otherwise        = 3/4 @@ turn

patternAppPortAngle :: Port -> Angle NumericType
patternAppPortAngle InputPortConst = 1/4 @@ turn
patternAppPortAngle ResultPortConst = 3/4 @@ turn
patternAppPortAngle port
  | isArgPort port = 1/4 @@ turn
  | otherwise        = 3/4 @@ turn

multiIfPortAngle :: Port -> Angle NumericType
multiIfPortAngle InputPortConst = 1/4 @@ turn
multiIfPortAngle ResultPortConst = 3/4 @@ turn
multiIfPortAngle port
  | isArgPort port = 1/4 @@ turn
  | otherwise        = 3/4 @@ turn

listCompPortAngle :: Port -> Angle NumericType
listCompPortAngle InputPortConst = 1/4 @@ turn
listCompPortAngle ResultPortConst = 3/4 @@ turn
listCompPortAngle port
  | isQualPort port = 1/4 @@ turn
  | isArgPort port  = 1/4 @@ turn
  | otherwise       = 3/4 @@ turn

nestedMultiIfPortAngle ::
  IconInfo
  -> [Maybe NamedIcon]
  -> Port
  -> Maybe NodeName
  -> Angle NumericType
nestedMultiIfPortAngle iconInfo args port maybeNodeName = case maybeNodeName of
  Nothing -> multiIfPortAngle port
  Just name -> case findIcon iconInfo name args of
    Nothing -> 3/4 @@ turn
    -- TODO Don't use hardcoded numbers
    -- The arguments correspond to ports [0, 2, 3, 4 ...]
    Just (_, icon) -> subAngle where
        subAngle = getPortAngleHelper True iconInfo icon port Nothing


generalNestedPortAngle ::
  IconInfo
  -> (Port -> Angle NumericType)
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> Port 
  -> Maybe NodeName 
  -> Angle NumericType
generalNestedPortAngle iconInfo defaultAngle headIcon args port maybeNodeName =
  case maybeNodeName of
    Nothing -> defaultAngle port
    Just name -> case findIcon iconInfo name (headIcon : args) of
      Nothing -> 1/8 @@ turn
      Just (_, icon) -> getPortAngleHelper True iconInfo icon port Nothing

getPortAngle :: IconInfo -> Icon -> Port -> Maybe NodeName -> Angle NumericType
getPortAngle = getPortAngleHelper False

getPortAngleHelper :: Bool -> IconInfo -> Icon -> Port -> Maybe NodeName -> Angle NumericType
getPortAngleHelper isEmbedded iconInfo (Icon icon _) port maybeNodeName = case icon of
  TextBoxIcon {} -> 3/4 @@ turn
  BindTextBoxIcon {} -> 1/4 @@ turn
  CaseResultIcon -> 3/4 @@ turn
  FunctionArgIcon {} -> lambdaPortAngle isEmbedded port
  FunctionDefIcon {} -> lambdaPortAngle isEmbedded port
  NestedApply _ headIcon args
    -> generalNestedPortAngle
      iconInfo
      (if not isEmbedded then applyPortAngle else nestedApplyPortAngle)
      -- TODO Refactor with iconToDiagram
      (findMaybeIconFromName iconInfo headIcon)
      (findMaybeIconsFromNames iconInfo args)
      port
      maybeNodeName
  NestedPatternApp headIcon args _rhsNodeName
    -> generalNestedPortAngle
      iconInfo
      patternAppPortAngle
      (laValue headIcon)
      (fmap laValue args)
      port
      maybeNodeName
  NestedCaseIcon _styleTag args
    -> nestedMultiIfPortAngle
      iconInfo
      (findMaybeIconsFromNames iconInfo args)
      port
      maybeNodeName
  ListCompIcon {} -> listCompPortAngle port -- TODO better angles for ListCompIcon 
  ListLitIcon {} -> applyPortAngle port -- TODO better angles for ListLitIcon 