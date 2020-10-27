{-# LANGUAGE PatternSynonyms #-}

module SyntaxNodeToIcon(
  nodeToIcon
) where

import Data.List(find)
import qualified Data.Set as Set

import           PortConstants(
  inputPort
  , argumentPorts
  , pattern PatternValuePortConst
  , listFromPort
  , listThenPort
  , listToPort
  )
import           Types(
  Labeled(..)
  , Icon(..)
  , DiagramIcon(..)
  , SyntaxNode(..)
  , SyntaxNodeCore(..)
  , Edge(..)
  , NameAndPort(..)
  , SgNamedNode
  , NodeName(..)
  , Port
  , LikeApplyFlavor(..)
  , CaseOrMultiIfTag(..)
  , Embedder(..)
  , Named(..)
  , EmbedderSyntaxNode
  , SrcRef
  , FuncDefRegionInfo
  )
import Util(maybeBoolToBool)

nodeToIcon :: EmbedderSyntaxNode -> Icon
nodeToIcon (Embedder embeddedNodes (SyntaxNode node src)) = case node of
  (ApplyNode flavor x)              -> nestedApplySyntaxNodeToIcon flavor x embeddedNodes src
  (PatternApplyNode s children)     -> nestedPatternNodeToIcon s children embeddedNodes src
  (BindNameNode s)                  -> Icon (BindTextBoxIcon s) src
  (LiteralNode s)                   -> Icon (TextBoxIcon s) src
  (FunctionArgNode labels)          -> functionArgIcon labels src
  (FunctionValueNode str funcDefRegionInfo) -> functionDefIcon embeddedNodes str funcDefRegionInfo src
  CaseResultNode                    -> Icon (CaseResultIcon) src
  (CaseOrMultiIfNode tag x)         -> nestedCaseOrMultiIfToIcon tag x embeddedNodes src
  (ListCompNode)                    -> Icon ListCompIcon src -- TODO actualy embede nodes
  (ListGenNode hasThen hasTo)       -> listGenNodeToIcon embeddedNodes hasThen hasTo src

-- listCompIcon embeddedNodes =  
--   ListCompIcon argList
--   where
--     dummyNode = ListCompNode
--     argPorts = take (Set.size embeddedNodes) (argumentPorts dummyNode)
--     argList = fmap (makeArg embeddedNodes) argPorts

-- | Helper for makeArg
findArg :: Port -> (NodeName, Edge) -> Bool
findArg currentPort
  (argName
  , Edge _ (NameAndPort fromName fromPort, NameAndPort toName toPort))
  | argName == fromName = currentPort == toPort
  | argName == toName = currentPort == fromPort
  | otherwise = False -- This case should never happen

makeArg :: Set.Set (NodeName, Edge) -> Port -> Maybe NodeName
makeArg args port = fst <$> find (findArg port) args

nestedApplySyntaxNodeToIcon :: LikeApplyFlavor
                            -> Int
                            -> Set.Set (NodeName, Edge)
                            -> SrcRef
                            -> Icon
nestedApplySyntaxNodeToIcon flavor numArgs args src =
  Icon (NestedApply flavor headIcon argList) src
  where
    dummyNode = SyntaxNode (ApplyNode flavor numArgs) src
    argPorts = take numArgs (argumentPorts dummyNode)
    headIcon = makeArg args (inputPort dummyNode)
    argList = fmap (makeArg args) argPorts

functionArgIcon ::
  [String]  -- labels
  -> SrcRef
  -> Icon
functionArgIcon labels src=
  Icon (FunctionArgIcon labels) src 

functionDefIcon ::
  Set.Set (NodeName, Edge)  -- embedded icons
  -> String -- name
  -> FuncDefRegionInfo
  -> SrcRef
  -> Icon
functionDefIcon embeddedNodes str funcDefRegionInfo src =
  Icon (FunctionDefIcon str funcDefRegionInfo embeddedBodyNode) src
  where
    dummyNode = SyntaxNode (FunctionValueNode str (Set.empty, 0)) src
    embeddedBodyNode = makeArg embeddedNodes (inputPort dummyNode)

nestedCaseOrMultiIfToIcon ::
  CaseOrMultiIfTag
  -> Int
  -> Set.Set (NodeName, Edge)
  -> SrcRef
  -> Icon
nestedCaseOrMultiIfToIcon styleTag numArgs args src 
  = Icon (NestedCaseIcon styleTag argList) src
  where
    dummyNode = SyntaxNode (CaseOrMultiIfNode CaseTag numArgs) src
    argPorts = take (2 * numArgs) $ argumentPorts dummyNode
    argList = fmap (makeArg args) (inputPort dummyNode : argPorts)

nestedPatternNodeToIcon :: 
  String
  -> [Labeled (Maybe SgNamedNode)]
  -> Set.Set (NodeName, Edge)
  -> SrcRef
  -> Icon
nestedPatternNodeToIcon str children args src = Icon (
    NestedPatternApp
    icon
    -- Why so many fmaps?
    ( (fmap . fmap . fmap . fmap) nodeToIcon children)
    asigendValueName
  )
  src
  where
    icon = pure $ Just (Named (NodeName (-1)) (Icon (TextBoxIcon str) src))
    asigendValueName = makeArg args PatternValuePortConst

listGenNodeToIcon ::
  Set.Set (NodeName, Edge)
  -> Bool
  -> Bool
  -> SrcRef
  -> Icon
listGenNodeToIcon args hasThen hasTo src = Icon diagramIcon src where
  diagramIcon = ListGenIcon 
    (makeArg args listFromPort)
    (if hasThen then Just (makeArg args listThenPort) else Nothing)
    (if hasTo then Just (makeArg args listToPort) else Nothing)

