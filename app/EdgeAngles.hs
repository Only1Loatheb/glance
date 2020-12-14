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
import Data.Maybe (isNothing, isJust)
import Types  (
  NumericType
  , Icon(..)
  , DiagramIcon(..) 
  , NodeName(..)
  , Port(..)
  , NamedIcon
  , IconInfo
  )

import Icons(findIcon,findMaybeIconFromName,findMaybeIconsFromNames)

import PortConstants( 
    pattern InputPort
  , pattern ResultPort
  , pattern PatternUnpackingPort
  , isArgPort
  , isQualPort
  )

data EmbedderType = Some | Case deriving (Show, Eq)
type EmbeddedIn = Maybe EmbedderType 

applyPortAngle :: Port -> Angle NumericType
applyPortAngle InputPort = 1/2 @@ turn -- input function
applyPortAngle ResultPort = 3/4 @@ turn
applyPortAngle _isInput = 1/4 @@ turn

nestedApplyPortAngle :: Port -> Angle NumericType
nestedApplyPortAngle InputPort = 1/2 @@ turn -- input function
nestedApplyPortAngle ResultPort = 3/4 @@ turn
nestedApplyPortAngle _isInput = 3/16 @@ turn

lambdaPortAngle :: EmbeddedIn -> Port -> Angle NumericType
lambdaPortAngle _ InputPort = 1/4 @@ turn
lambdaPortAngle _ ResultPort = 3/4 @@ turn
lambdaPortAngle embeddedIn port
  | isArgPort port = if isJust embeddedIn then 1/2 @@ turn else 1/4 @@ turn
  | otherwise      = 3/4 @@ turn

patternAppPortAngle :: Port -> Angle NumericType
patternAppPortAngle InputPort = 1/4 @@ turn
patternAppPortAngle ResultPort = 3/4 @@ turn
patternAppPortAngle port
  | isArgPort port = 1/4 @@ turn
  | otherwise      = 3/4 @@ turn

flipedPatternAngles :: Port -> Angle NumericType
flipedPatternAngles InputPort = 1/4 @@ turn
flipedPatternAngles ResultPort = 3/4 @@ turn
flipedPatternAngles port
  | isArgPort port = 1/4 @@ turn
  | otherwise      = 1/4 @@ turn

multiIfPortAngle :: Port -> Angle NumericType
multiIfPortAngle InputPort = 1/4 @@ turn
multiIfPortAngle ResultPort = 3/4 @@ turn
multiIfPortAngle port
  | isArgPort port = 1/4 @@ turn
  | otherwise      = 3/4 @@ turn

listCompPortAngle :: Port -> Angle NumericType
listCompPortAngle InputPort = 1/4 @@ turn
listCompPortAngle ResultPort = 3/4 @@ turn
listCompPortAngle port
  | isQualPort port = 1/4 @@ turn
  | isArgPort port  = 1/4 @@ turn
  | otherwise       = 3/4 @@ turn

listLitPortAngle :: Port -> Angle NumericType
listLitPortAngle InputPort = 1/2 @@ turn -- input function
listLitPortAngle ResultPort = 3/4 @@ turn
listLitPortAngle PatternUnpackingPort = 1/4 @@ turn
listLitPortAngle _isInput = 1/4 @@ turn

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
  TextBoxIcon {} -> if port == ResultPort then 3/4 @@ turn else 1/4 @@ turn
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
      (Nothing)
      ([])
      port
      maybeNodeName
      embeddedIn
  NestedCaseIcon _ arg condsAndVals ->
    let 
      (conds, vals) = unzip condsAndVals
      subicons = findMaybeIconFromName iconInfo arg : findMaybeIconsFromNames iconInfo (conds ++ vals)
    in nestedMultiIfPortAngle iconInfo subicons  port maybeNodeName
  ListCompIcon {} -> listCompPortAngle port -- TODO better angles for ListCompIcon 
  ListLitIcon {} -> listLitPortAngle port -- TODO better angles for ListLitIcon 