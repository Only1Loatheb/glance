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
  Named(..)
  , NameAndPort
  , NumericType
  , Icon(..)
  , DiagramIcon(..) 
  , NodeName(..)
  , Port(..)
  , NamedIcon
  , IconInfo
  )

import Icons(findMaybeIconFromName)

import PortConstants( 
    pattern InputPort
  , pattern ResultPort
  , pattern PatternUnpackingPort
  , pattern FunDefValuePort
  , isFunDefValuePort
  , isArgPort
  , isQualPort
  )

data EmbedderType = Some | Case deriving (Show, Eq)
type EmbeddedIn = Maybe EmbedderType 

defaultDirection :: Port -> Angle NumericType
defaultDirection port = if isArgPort port then 0 @@ turn else 1/2 @@ turn

defaultEmbeddedAngle :: EmbeddedIn ->  Port -> Angle NumericType
defaultEmbeddedAngle (Just Case) _ = 0 @@ turn 
defaultEmbeddedAngle _ port = if isArgPort port then 0 @@ turn else 1/2 @@ turn

applyPortAngle :: Port -> Angle NumericType
applyPortAngle port
  | InputPort <- port = 1/4 @@ turn
  | ResultPort <- port = 1/2 @@ turn
  | otherwise = 0 @@ turn

nestedApplyPortAngle :: Port -> Angle NumericType
nestedApplyPortAngle InputPort = 1/4 @@ turn -- input function
nestedApplyPortAngle ResultPort = 1/2 @@ turn
nestedApplyPortAngle _isInput = 1/16 @@ turn

lambdaPortAngle :: EmbeddedIn -> Port -> Angle NumericType
lambdaPortAngle _ FunDefValuePort = 0 @@ turn
lambdaPortAngle _ ResultPort = 1/2 @@ turn
lambdaPortAngle embeddedIn port
  | isArgPort port = if isJust embeddedIn then 1/4 @@ turn else 0 @@ turn
  | otherwise      = 1/2 @@ turn

patternAppPortAngle :: Port -> Angle NumericType
patternAppPortAngle InputPort = 0 @@ turn
patternAppPortAngle ResultPort = 1/2 @@ turn
patternAppPortAngle PatternUnpackingPort = 0 @@ turn
patternAppPortAngle port = defaultDirection port

casePortAngle :: Port -> Angle NumericType
casePortAngle ResultPort = 1/2 @@ turn
casePortAngle InputPort = 0 @@ turn
casePortAngle _ = 0 @@ turn


listCompPortAngle :: Port -> Angle NumericType
listCompPortAngle InputPort = 0 @@ turn
listCompPortAngle ResultPort = 1/2 @@ turn
listCompPortAngle port
  | isQualPort port = 0 @@ turn
  | isArgPort port  = 0 @@ turn
  | otherwise       = 1/2 @@ turn

listLitPortAngle :: Port -> Angle NumericType
listLitPortAngle InputPort = 0 @@ turn -- input function
listLitPortAngle ResultPort = 1/2 @@ turn
listLitPortAngle PatternUnpackingPort = 0 @@ turn
listLitPortAngle _isInput = 0 @@ turn

getPortAngle :: IconInfo -> NodeName -> NameAndPort -> Angle NumericType
getPortAngle iconInfo parentName (Named childName port) = foundAngle where
  foundAngle
    | Just parrentIcon <- findMaybeIconFromName iconInfo (Just parentName) 
      = getPortAngleHelper Nothing iconInfo parrentIcon port childName
    | otherwise = defaultDirection port

nestedPortAngle :: EmbeddedIn -> IconInfo -> Angle NumericType -> Port -> Maybe NodeName -> Angle NumericType
nestedPortAngle embeddedIn iconInfo defaultAngle port maybeNodeName = foundAngle where
  foundAngle = case findMaybeIconFromName iconInfo maybeNodeName of
    Nothing -> defaultAngle
    Just icon@(Named name _) -> getPortAngleHelper embeddedIn iconInfo icon port name

getPortAngleHelper :: EmbeddedIn -> IconInfo -> NamedIcon -> Port -> NodeName -> Angle NumericType
getPortAngleHelper embeddedIn iconInfo (Named iconName (Icon icon _)) port nodeName = foundAngle where
  foundAngle 
    | nodeName == iconName , Just Case <- embeddedIn = case (icon,port) of
      (NestedApply {},InputPort) -> 3/8 @@ turn
      _ -> 0 @@ turn
    | nodeName == iconName = case icon of
      TextBoxIcon {} -> defaultDirection port
      FunctionArgIcon {} -> 1/2 @@ turn
      FunctionDefIcon {} -> lambdaPortAngle embeddedIn port
      BindTextBoxIcon {} -> 0 @@ turn
      NestedApply {} -> (if isNothing embeddedIn then applyPortAngle else nestedApplyPortAngle) port
      NestedPatternApp {} -> patternAppPortAngle port
      NestedCaseIcon {} -> casePortAngle port
      CaseResultIcon {}-> defaultDirection port
      ListCompIcon {} -> listCompPortAngle port 
      ListLitIcon {} -> listLitPortAngle port
      Concentrator {} -> 0 @@ turn 
    | otherwise = nestedPortAngle newEmbedded iconInfo defaultAngle port (Just nodeName) where
      defaultAngle = defaultEmbeddedAngle newEmbedded port
      newEmbedded = newEmbededIn embeddedIn icon

newEmbededIn :: EmbeddedIn -> DiagramIcon -> EmbeddedIn
newEmbededIn _ NestedCaseIcon {} = Just Case 
newEmbededIn _ CaseResultIcon {} = Just Case 
newEmbededIn _ ListCompIcon {} = Nothing
newEmbededIn (Just Case) _ = Just Case
newEmbededIn _ _ = Just Some



