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
import Data.Maybe (isNothing, isJust)

data EmbedderType = Some | Case deriving (Show, Eq)
type EmbeddedIn = Maybe EmbedderType 

applyPortAngle :: Port -> Angle NumericType
applyPortAngle InputPortConst = 1/2 @@ turn -- input function
applyPortAngle ResultPortConst = 3/4 @@ turn
applyPortAngle _isInput = 1/4 @@ turn

nestedApplyPortAngle :: Port -> Angle NumericType
nestedApplyPortAngle InputPortConst = 1/2 @@ turn -- input function
nestedApplyPortAngle ResultPortConst = 3/4 @@ turn
nestedApplyPortAngle _isInput = 3/16 @@ turn

lambdaPortAngle :: EmbeddedIn -> Port -> Angle NumericType
lambdaPortAngle _ InputPortConst = 1/4 @@ turn
lambdaPortAngle _ ResultPortConst = 3/4 @@ turn
lambdaPortAngle embeddedIn port
  | isArgPort port = if isJust embeddedIn then 1/2 @@ turn else 1/4 @@ turn
  | otherwise      = 3/4 @@ turn

patternAppPortAngle :: Port -> Angle NumericType
patternAppPortAngle InputPortConst = 1/4 @@ turn
patternAppPortAngle ResultPortConst = 3/4 @@ turn
patternAppPortAngle port
  | isArgPort port = 1/4 @@ turn
  | otherwise      = 3/4 @@ turn

flipedPatternAngles :: Port -> Angle NumericType
flipedPatternAngles InputPortConst = 1/4 @@ turn
flipedPatternAngles ResultPortConst = 3/4 @@ turn
flipedPatternAngles port
  | isArgPort port = 1/4 @@ turn
  | otherwise      = 1/4 @@ turn

multiIfPortAngle :: Port -> Angle NumericType
multiIfPortAngle InputPortConst = 1/4 @@ turn
multiIfPortAngle ResultPortConst = 3/4 @@ turn
multiIfPortAngle port
  | isArgPort port = 1/4 @@ turn
  | otherwise      = 3/4 @@ turn

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
    Nothing -> 1/4 @@ turn
    -- TODO Don't use hardcoded numbers
    -- The arguments correspond to ports [0, 2, 3, 4 ...]
    Just (_, icon) -> subAngle where
        subAngle = getPortAngleHelper (Just Case) iconInfo icon port Nothing


generalNestedPortAngle ::
  IconInfo
  -> (Port -> Angle NumericType)
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> Port 
  -> Maybe NodeName
  -> EmbeddedIn 
  -> Angle NumericType
generalNestedPortAngle iconInfo defaultAngle headIcon args port maybeNodeName embeddedIn =
  case maybeNodeName of
    Nothing -> defaultAngle port
    Just name -> case findIcon iconInfo name (headIcon : args) of
      Nothing -> 1/8 @@ turn
      Just (_, icon) -> getPortAngleHelper embeddedIn iconInfo icon port Nothing

getPortAngle :: IconInfo -> Icon -> Port -> Maybe NodeName -> Angle NumericType
getPortAngle = getPortAngleHelper Nothing

getPortAngleHelper :: EmbeddedIn -> IconInfo -> Icon -> Port -> Maybe NodeName -> Angle NumericType
getPortAngleHelper embeddedIn iconInfo (Icon icon _) port maybeNodeName = case icon of
  TextBoxIcon {} -> 3/4 @@ turn
  BindTextBoxIcon {} -> 1/4 @@ turn
  CaseResultIcon -> 1/4 @@ turn
  FunctionArgIcon {} -> lambdaPortAngle embeddedIn port
  FunctionDefIcon {} -> lambdaPortAngle embeddedIn port
  NestedApply _ headIcon args
    -> generalNestedPortAngle
      iconInfo
      (if isNothing embeddedIn then applyPortAngle else nestedApplyPortAngle)
      -- TODO Refactor with iconToDiagram
      (findMaybeIconFromName iconInfo headIcon)
      (findMaybeIconsFromNames iconInfo args)
      port
      maybeNodeName
      embeddedIn
  NestedPatternApp headIcon args _rhsNodeName
    -> generalNestedPortAngle
      iconInfo
      (case embeddedIn of {Just Case -> flipedPatternAngles; _ -> patternAppPortAngle})
      (laValue headIcon)
      (fmap laValue args)
      port
      maybeNodeName
      embeddedIn
  NestedCaseIcon _ arg condsAndVals ->
    let 
      (conds, vals) = unzip condsAndVals
      subicons = findMaybeIconFromName iconInfo arg : findMaybeIconsFromNames iconInfo (conds ++ vals)
    in nestedMultiIfPortAngle iconInfo subicons  port maybeNodeName
  ListCompIcon {} -> listCompPortAngle port -- TODO better angles for ListCompIcon 
  ListLitIcon {} -> applyPortAngle port -- TODO better angles for ListLitIcon 