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
  Icon(..)
  , DiagramIcon(..) 
  , SpecialNum
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

{-# ANN module "HLint: ignore Use record patterns" #-}
{-# ANN module "HLint: ignore Unnecessary hiding" #-}

applyPortAngle :: Floating n => Port -> Angle n
applyPortAngle InputPortConst = 1/2 @@ turn -- input function
applyPortAngle ResultPortConst = 3/4 @@ turn
applyPortAngle _isInput = 1/4 @@ turn

nestedApplyPortAngle :: Floating n => Port -> Angle n
nestedApplyPortAngle InputPortConst = 1/2 @@ turn -- input function
nestedApplyPortAngle ResultPortConst = 3/4 @@ turn
nestedApplyPortAngle _isInput = 3/16 @@ turn

lambdaPortAngle :: Floating n => Bool -> Port -> Angle n
lambdaPortAngle _ InputPortConst = 1/4 @@ turn
lambdaPortAngle _ ResultPortConst = 3/4 @@ turn
lambdaPortAngle isEmbedded port
  | isArgPort port = if isEmbedded then 1/2 @@ turn else 1/4 @@ turn
  | otherwise        = 3/4 @@ turn

patternAppPortAngle :: Floating n => Port -> Angle n
patternAppPortAngle InputPortConst = 1/4 @@ turn
patternAppPortAngle ResultPortConst = 3/4 @@ turn
patternAppPortAngle port
  | isArgPort port = 1/4 @@ turn
  | otherwise        = 3/4 @@ turn

multiIfPortAngle :: Floating n => Port -> Angle n
multiIfPortAngle InputPortConst = 1/4 @@ turn
multiIfPortAngle ResultPortConst = 3/4 @@ turn
multiIfPortAngle port
  | isArgPort port = 1/4 @@ turn
  | otherwise        = 3/4 @@ turn

listCompPortAngle :: Floating n => Port -> Angle n
listCompPortAngle InputPortConst = 1/4 @@ turn
listCompPortAngle ResultPortConst = 3/4 @@ turn
listCompPortAngle port
  | isQualPort port = 1/4 @@ turn
  | isArgPort port  = 1/4 @@ turn
  | otherwise       = 3/4 @@ turn

nestedMultiIfPortAngle :: SpecialNum n
  => IconInfo
  -> [Maybe NamedIcon]
  -> Port
  -> Maybe NodeName
  -> Angle n
nestedMultiIfPortAngle iconInfo args port maybeNodeName = case maybeNodeName of
  Nothing -> multiIfPortAngle port
  Just name -> case findIcon iconInfo name args of
    Nothing -> 3/4 @@ turn
    -- TODO Don't use hardcoded numbers
    -- The arguments correspond to ports [0, 2, 3, 4 ...]
    Just (_, icon) -> subAngle where
        subAngle = getPortAngleHelper True iconInfo icon port Nothing


generalNestedPortAngle :: SpecialNum n
  => IconInfo
  -> (Port -> Angle n)
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> Port 
  -> Maybe NodeName 
  -> Angle n
generalNestedPortAngle iconInfo defaultAngle headIcon args port maybeNodeName =
  case maybeNodeName of
    Nothing -> defaultAngle port
    Just name -> case findIcon iconInfo name (headIcon : args) of
      Nothing -> 1/8 @@ turn
      Just (_, icon) -> getPortAngleHelper True iconInfo icon port Nothing

getPortAngle :: SpecialNum n
  => IconInfo -> Icon -> Port -> Maybe NodeName -> Angle n
getPortAngle = getPortAngleHelper False

getPortAngleHelper :: SpecialNum n
  => Bool -> IconInfo -> Icon -> Port -> Maybe NodeName -> Angle n
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
  NestedPatternApp headIcon args rhsNodeName
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