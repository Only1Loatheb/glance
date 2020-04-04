{-# LANGUAGE PatternSynonyms #-}
module PortConstants
  ( pattern InputPortConst
  , pattern ResultPortConst
  , isInputPort
  , inputPort,
  resultPort,
  argumentPorts,
  caseValuePorts,
  casePatternPorts,
  multiIfValuePorts,
  multiIfBoolPorts,
  argPortsConst,
  mixedPorts,
  resultPortsConst
  ) where

import Types( Port(..),
              SyntaxNode(..))

isInputPort :: Port -> Bool
isInputPort (Port portNo) = even portNo 

pattern InputPortConst :: Port
pattern InputPortConst = Port 0

pattern ResultPortConst :: Port
pattern ResultPortConst = Port 1

argPortsConst :: [Port]
argPortsConst = fmap Port [2,4..]

resultPortsConst :: [Port]
resultPortsConst = fmap Port [3,5..]

caseValuePorts :: [Port]
caseValuePorts = resultPortsConst

casePatternPorts :: [Port]
casePatternPorts = argPortsConst

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
  (ApplyNode _ _) -> argPortsConst
  PatternApplyNode _ _-> mixedPorts
  (FunctionDefNode _ _ _) -> resultPortsConst
  CaseOrMultiIfNode _ _ -> mixedPorts
  NameNode _ -> []
  BindNameNode _ -> []
  LiteralNode _ -> []
  CaseResultNode -> []
