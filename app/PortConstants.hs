{-# LANGUAGE PatternSynonyms #-}
module PortConstants
  ( pattern InputPortConst
  , pattern ResultPortConst
  , pattern PatternValuePortConst
  , isInputPort
  , inputPort
  , resultPort
  , argumentPorts
  , caseValuePorts
  , caseConditionPorts
  , multiIfValuePorts
  , multiIfBoolPorts
  , argPortsConst
  , mixedPorts
  , resultPortsConst
  ) where

import Types( Port(..),
              SyntaxNode(..))

isInputPort :: Port -> Bool
isInputPort (Port portNo) = even portNo 

pattern InputPortConst :: Port
pattern InputPortConst = Port 0

pattern ResultPortConst :: Port
pattern ResultPortConst = Port 1

pattern PatternValuePortConst :: Port
pattern PatternValuePortConst = Port 2

argPortsConst :: [Port]
argPortsConst = fmap Port [2,4..]

resultPortsConst :: [Port]
resultPortsConst = fmap Port [3,5..]

caseValuePorts :: [Port]
caseValuePorts = resultPortsConst

caseConditionPorts :: [Port]
caseConditionPorts = argPortsConst

multiIfValuePorts :: [Port]
multiIfValuePorts = resultPortsConst

multiIfBoolPorts :: [Port]
multiIfBoolPorts = argPortsConst

mixedPorts :: [Port]
mixedPorts = fmap Port [2,3..]

-- TODO It's a bit strange that the parameter is a SyntaxNode, not an Icon.
inputPort :: SyntaxNode -> Port
inputPort = const InputPortConst

resultPort :: SyntaxNode -> Port
resultPort = const ResultPortConst

argumentPorts :: SyntaxNode -> [Port]
argumentPorts n = case n of
  ApplyNode {} -> argPortsConst
  PatternApplyNode {} -> resultPortsConst
  FunctionValueNode {} -> resultPortsConst
  CaseOrMultiIfNode {} -> mixedPorts
  NameNode {} -> []
  BindNameNode {} -> []
  LiteralNode {} -> []
  CaseResultNode {}-> []
  ListCompNode {} -> argPortsConst
