{-# LANGUAGE PatternSynonyms #-}
module PortConstants
  ( pattern InputPort
  , pattern ResultPort
  , pattern PatternUnpackingPort
  , pattern FunDefValuePort
  , isArgPort
  , inputPort
  , resultPort
  , argumentPortList
  , caseValuePorts
  , caseCondPortList
  , argPortList
  , casePortPairList
  , valuePortList
  , listCompQualPortList
  , isQualPort
  , isInputPort
  , isPatternUnpackingPort
  , isResultPort
  , isFunDefValuePort
  ) where

import Types( Port(..)
  , SyntaxNode(..)
  , SyntaxNodeCore(..)
  )

isInputPort :: Port -> Bool
isInputPort InputPort = True
isInputPort _ = False

isResultPort :: Port -> Bool
isResultPort ResultPort = True
isResultPort _ = False

isPatternUnpackingPort :: Port -> Bool
isPatternUnpackingPort PatternUnpackingPort = True
isPatternUnpackingPort _ = False

isFunDefValuePort :: Port -> Bool
isFunDefValuePort FunDefValuePort = True
isFunDefValuePort _ = False

isArgPort :: Port -> Bool
isArgPort (Port portNo) = odd portNo

isQualPort :: Port -> Bool
isQualPort (Port portNo) = portNo < 0

pattern InputPort :: Port
pattern InputPort = Port 1

pattern ResultPort :: Port
pattern ResultPort = Port 0

pattern PatternUnpackingPort :: Port
pattern PatternUnpackingPort = Port (-1)

pattern FunDefValuePort :: Port
pattern FunDefValuePort = Port (-3)

argPortList :: [Port]
argPortList = fmap Port [3,5..]

valuePortList :: [Port]
valuePortList = fmap Port [2,4..]

qualPortList :: [Port]
qualPortList = fmap Port [-2,-4..]

caseCondPortList :: [Port]
caseCondPortList = argPortList

caseValuePorts :: [Port]
caseValuePorts =  qualPortList

listCompQualPortList :: [Port]
listCompQualPortList = qualPortList


casePortPairList :: [(Port,Port)]
casePortPairList = zip caseCondPortList caseValuePorts

inputPort :: SyntaxNode -> Port
inputPort (SyntaxNode FunctionValueNode {} _) = FunDefValuePort
inputPort _ = InputPort

resultPort :: SyntaxNode -> Port
resultPort = const ResultPort

argumentPortList :: SyntaxNode -> [Port]
argumentPortList (SyntaxNode n _) = case n of
  ApplyNode {} -> argPortList
  PatternNode {} -> valuePortList
  FunctionValueNode {} -> valuePortList
  CaseNode {} -> caseCondPortList
  ListCompNode {} -> argPortList
  ListLitNode {} -> argPortList
  _ -> error "Node don't have argument ports. PortConstants argumentPortList"
