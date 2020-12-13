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
  , argPortsConst
  , mixedPorts
  , resultPortsConst
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

argPortsConst :: [Port]
argPortsConst = fmap Port [2,4..]

resultPortsConst :: [Port]
resultPortsConst = fmap Port [3,5..]

qualPortsConst :: [Port]
qualPortsConst = fmap Port [-2,-4..]

caseConditionPorts :: [Port]
caseConditionPorts = argPortsConst

caseValuePorts :: [Port]
caseValuePorts =  qualPortsConst

listCompQualPorts :: [Port]
listCompQualPorts = qualPortsConst


mixedPorts :: [(Port,Port)]
mixedPorts = zip caseConditionPorts caseValuePorts

-- TODO It's a bit strange that the parameter is a SyntaxNode, not an Icon.
inputPort :: SyntaxNode -> Port
inputPort = const InputPortConst

resultPort :: SyntaxNode -> Port
resultPort = const ResultPortConst

argumentPorts :: SyntaxNode -> [Port]
argumentPorts (SyntaxNode n _) = case n of
  ApplyNode {} -> argPortsConst
  PatternNode {} -> resultPortsConst
  FunctionValueNode {} -> resultPortsConst
  CaseNode {} -> caseConditionPorts
  ListCompNode {} -> argPortsConst
  ListLitNode {} -> argPortsConst
  _ -> error "Node don't have argument ports. PortConstants argumentPorts"
