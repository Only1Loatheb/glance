{-# LANGUAGE PatternSynonyms #-}
module PortConstants
  ( pattern InputPortConst
  , pattern ResultPortConst
  , pattern PatternUnpackingPort
  , isArgPort
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
  , listFromPort
  , listThenPort
  , listToPort
  , listCompQualPorts
  , isQualPort
  , isInputPort
  , isPatternUnpackingPort
  , isResultPort
  ) where

import Types( Port(..)
  , SyntaxNode(..)
  , SyntaxNodeCore(..)
  )

isInputPort :: Port -> Bool
isInputPort InputPortConst = True
isInputPort _ = False

isResultPort :: Port -> Bool
isResultPort ResultPortConst = True
isResultPort _ = False

isPatternUnpackingPort :: Port -> Bool
isPatternUnpackingPort PatternUnpackingPort = True
isPatternUnpackingPort _ = False

isArgPort :: Port -> Bool
isArgPort (Port portNo) = even portNo

isQualPort :: Port -> Bool
isQualPort (Port portNo) = portNo < 0

pattern InputPortConst :: Port
pattern InputPortConst = Port 0

pattern ResultPortConst :: Port
pattern ResultPortConst = Port 1

pattern PatternUnpackingPort :: Port
pattern PatternUnpackingPort = Port 2

listFromPort :: Port
listFromPort = Port 2

listThenPort :: Port
listThenPort = Port 4

listToPort :: Port
listToPort = Port 6

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

listCompQualPorts :: [Port]
listCompQualPorts = fmap Port [-2,-4..]


mixedPorts :: [Port]
mixedPorts = fmap Port [2,3..]

-- TODO It's a bit strange that the parameter is a SyntaxNode, not an Icon.
inputPort :: SyntaxNode -> Port
inputPort = const InputPortConst

resultPort :: SyntaxNode -> Port
resultPort = const ResultPortConst

argumentPorts :: SyntaxNode -> [Port]
argumentPorts (SyntaxNode n _) = case n of
  ApplyNode {} -> argPortsConst
  PatternApplyNode {} -> resultPortsConst
  FunctionValueNode {} -> resultPortsConst
  CaseOrMultiIfNode {} -> mixedPorts
  ListCompNode {} -> argPortsConst
  _ -> error "Node don't have argument ports. PortConstants argumentPorts"
