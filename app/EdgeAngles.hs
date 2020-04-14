{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EdgeAngles
    (
      getPortAngle
    ) where

import Diagrams.Prelude hiding ((&), (#), Name)


import Types  ( Icon(..)
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
  , isInputPort)

{-# ANN module "HLint: ignore Use record patterns" #-}
{-# ANN module "HLint: ignore Unnecessary hiding" #-}

applyPortAngle :: Floating n => Port -> Angle n
applyPortAngle InputPortConst = 1/2 @@ turn -- input function
applyPortAngle ResultPortConst = 3/4 @@ turn
applyPortAngle _isInput = 1/4 @@ turn

lambdaPortAngle :: Floating n => Port -> Angle n
lambdaPortAngle InputPortConst = 1/4 @@ turn
lambdaPortAngle ResultPortConst = 3/4 @@ turn
lambdaPortAngle port
  | isInputPort port = 1/2 @@ turn
  | otherwise        = 3/4 @@ turn

patternAppPortAngle :: Floating n => Port -> Angle n
patternAppPortAngle InputPortConst = 1/4 @@ turn
patternAppPortAngle ResultPortConst = 3/4 @@ turn
patternAppPortAngle port
  | isInputPort port = 1/4 @@ turn
  | otherwise        = 3/4 @@ turn

multiIfPortAngle :: Floating n => Port -> Angle n
multiIfPortAngle InputPortConst = 1/4 @@ turn
multiIfPortAngle ResultPortConst = 3/4 @@ turn
multiIfPortAngle port
  | isInputPort port = 1/4 @@ turn
  | otherwise        = 3/4 @@ turn

nestedMultiIfPortAngle :: SpecialNum n
  => IconInfo
  -> [Maybe NamedIcon]
  -> Port
  -> Maybe NodeName
  -> Angle n
nestedMultiIfPortAngle iconInfo args port maybeNodeName = case maybeNodeName of
  Nothing -> multiIfPortAngle port
  Just name -> case findIcon iconInfo name args of
    Nothing -> 0 @@ turn
    -- TODO Don't use hardcoded numbers
    -- The arguments correspond to ports [0, 2, 3, 4 ...]
    Just (argNum, icon) -> if odd argNum && argNum >= 1
      -- The icon will be reflected
      then reflectXAngle subAngle
      else subAngle
      where
        subAngle = getPortAngleHelper True iconInfo icon port Nothing


generalNestedPortAngle :: SpecialNum n
  => IconInfo
  -> (Port -> Angle n)
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> Port -> Maybe NodeName -> Angle n
generalNestedPortAngle iconInfo defaultAngle headIcon args port maybeNodeName =
  case maybeNodeName of
    Nothing -> defaultAngle port
    Just name -> case findIcon iconInfo name (headIcon : args) of
      Nothing -> 0 @@ turn
      Just (_, icon) -> getPortAngleHelper True iconInfo icon port Nothing

reflectXAngle :: SpecialNum n => Angle n -> Angle n
reflectXAngle x = reflectedAngle where
  normalizedAngle = normalizeAngle x
  reflectedAngle = (-) <$> halfTurn <*> normalizedAngle

getPortAngle :: SpecialNum n
  => IconInfo -> Icon -> Port -> Maybe NodeName -> Angle n
getPortAngle = getPortAngleHelper False

getPortAngleHelper :: SpecialNum n
  => Bool -> IconInfo -> Icon -> Port -> Maybe NodeName -> Angle n
getPortAngleHelper _embedded iconInfo icon port maybeNodeName = case icon of
  TextBoxIcon _ -> 1/4 @@ turn
  BindTextBoxIcon _ -> 1/4 @@ turn
  MultiIfIcon _ -> multiIfPortAngle port
  CaseIcon _ -> multiIfPortAngle port
  CaseResultIcon -> 1/4 @@ turn
  LambdaIcon _ _ _ -> lambdaPortAngle port
  NestedApply _ headIcon args
    -> generalNestedPortAngle
      iconInfo
      applyPortAngle
      -- TODO Refactor with iconToDiagram
      (findMaybeIconFromName iconInfo headIcon)
      (findMaybeIconsFromNames iconInfo args)
      port
      maybeNodeName
  NestedPatternApp headIcon args
    -> generalNestedPortAngle
      iconInfo
      patternAppPortAngle
      (laValue headIcon)
      (fmap laValue args)
      port
      maybeNodeName
  NestedCaseIcon args
    -> nestedMultiIfPortAngle
      iconInfo
      (findMaybeIconsFromNames iconInfo args)
      port
      maybeNodeName
  NestedMultiIfIcon args
    -> nestedMultiIfPortAngle
      iconInfo
      (findMaybeIconsFromNames iconInfo args)
      port
      maybeNodeName