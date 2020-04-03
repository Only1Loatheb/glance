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
              , PortNo(..)
              , NamedIcon
              , Labeled(..)
              , IconInfo
              )                         
import Icons(findIconFromName,findIcon)

{-# ANN module "HLint: ignore Use record patterns" #-}
{-# ANN module "HLint: ignore Unnecessary hiding" #-}

applyPortAngle :: Floating n => Port -> Angle n
applyPortAngle (Port (PortNo x) isInput) =  case x of
  0 -> 1/4 @@ turn -- input
  1 -> 3/4 @@ turn -- output
  _ -> 1/4 @@ turn -- [idk, side fromline comes with value like in lambda]

lambdaPortAngle :: Floating n => Port -> Angle n
lambdaPortAngle (Port (PortNo x) isInput) =  case x of
  0 -> 1/4 @@ turn -- input
  1 -> 3/4 @@ turn-- output
  _ -> 0 @@ turn -- value placement

patternAppPortAngle :: Floating n => Port -> Angle n
patternAppPortAngle (Port (PortNo x) isInput) = case x of
  0 -> 1/4 @@ turn -- input
  1 -> 1/4 @@ turn -- result label input
  _ -> 3/4 @@ turn -- output of inner constructors from pattern

multiIfPortAngle :: Floating n => Port -> Angle n
multiIfPortAngle (Port (PortNo x) isInput) = case x of
  0 -> 1/4 @@ turn -- input
  1 -> 3/4 @@ turn -- output 
  _ -> otherAngle where otherAngle -- options, may get fliped
                           | even x = 1/4 @@ turn -- then side
                           | otherwise = 1/2 @@ turn -- if side

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
  LambdaIcon _ _ _ _ -> lambdaPortAngle port
  NestedApply _ headIcon args
    -> generalNestedPortAngle
      iconInfo
      applyPortAngle
      -- TODO Refactor with iconToDiagram
      (fmap (findIconFromName iconInfo) headIcon)
      ((fmap . fmap) (findIconFromName iconInfo) args)
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
      ((fmap . fmap) (findIconFromName iconInfo) args)
      port
      maybeNodeName
  NestedMultiIfIcon args
    -> nestedMultiIfPortAngle
      iconInfo
      ((fmap . fmap) (findIconFromName iconInfo) args)
      port
      maybeNodeName