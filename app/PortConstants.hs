{-# LANGUAGE PatternSynonyms #-}
module PortConstants
  ( pattern InputPort
  , pattern ResultPort
  , pattern PatternUnpackingPort
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

isArgPort :: Port -> Bool
isArgPort (Port portNo) = even portNo

isQualPort :: Port -> Bool
isQualPort (Port portNo) = portNo < 0

pattern InputPort :: Port
pattern InputPort = Port 0

pattern ResultPort :: Port
pattern ResultPort = Port 1

pattern PatternUnpackingPort :: Port
pattern PatternUnpackingPort = Port (-1)

argPortList :: [Port]
argPortList = fmap Port [2,4..]

valuePortList :: [Port]
valuePortList = fmap Port [3,5..]

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

-- TODO It's a bit strange that the parameter is a SyntaxNode, not an Icon.
inputPort :: SyntaxNode -> Port
inputPort = const InputPort

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
