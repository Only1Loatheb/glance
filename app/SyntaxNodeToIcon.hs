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
  )
import           Types(
  Labeled(..)
  , Icon(..)
  , SyntaxNode(..)
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
  )
import Util(maybeBoolToBool)

nodeToIcon :: EmbedderSyntaxNode -> Icon
nodeToIcon (Embedder embeddedNodes node) = case node of
  (ApplyNode flavor x)
    -> nestedApplySyntaxNodeToIcon flavor x embeddedNodes
  (PatternApplyNode s children)
    -> nestedPatternNodeToIcon s children embeddedNodes
  (NameNode s) -> TextBoxIcon s
  (BindNameNode s) -> BindTextBoxIcon s
  (LiteralNode s) -> TextBoxIcon s
  (FunctionArgNode labels)
    -> functionArgIcon labels
  (FunctionValueNode str bodyNodes)
    -> functionDefIcon embeddedNodes str bodyNodes
  CaseResultNode -> CaseResultIcon
  (CaseOrMultiIfNode tag x)
    -> nestedCaseOrMultiIfNodeToIcon tag x embeddedNodes
  (ListCompNode) -> ListCompIcon -- TODO actualy embede nodes

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
  | argName == fromName = maybeBoolToBool $ fmap (== currentPort) toPort
  | argName == toName = maybeBoolToBool $ fmap (== currentPort) fromPort
  | otherwise = False -- This case should never happen

makeArg :: Set.Set (NodeName, Edge) -> Port -> Maybe NodeName
makeArg args port = fst <$> find (findArg port) args

nestedApplySyntaxNodeToIcon :: LikeApplyFlavor
                            -> Int
                            -> Set.Set (NodeName, Edge)
                            -> Icon
nestedApplySyntaxNodeToIcon flavor numArgs args =
  NestedApply flavor headIcon argList
  where
    dummyNode = ApplyNode flavor numArgs
    argPorts = take numArgs (argumentPorts dummyNode)
    headIcon = makeArg args (inputPort dummyNode)
    argList = fmap (makeArg args) argPorts

functionArgIcon ::
  [String]  -- labels
  -> Icon
functionArgIcon labels =
  FunctionArgIcon labels 

functionDefIcon ::
  Set.Set (NodeName, Edge)  -- embedded icons
  -> Maybe String -- name
  -> Set.Set NodeName  -- body nodes
  -> Icon
functionDefIcon embeddedNodes str bodyNodes=
  FunctionDefIcon str bodyNodes embeddedBodyNode
  where
    dummyNode = FunctionValueNode str Set.empty
    embeddedBodyNode = makeArg embeddedNodes (inputPort dummyNode)

nestedCaseOrMultiIfNodeToIcon ::
  CaseOrMultiIfTag
  -> Int
  -> Set.Set (NodeName, Edge)
  -> Icon
nestedCaseOrMultiIfNodeToIcon tag numArgs args = case tag of
  CaseTag -> NestedCaseIcon argList
  MultiIfTag -> NestedMultiIfIcon argList
  where
    dummyNode = CaseOrMultiIfNode CaseTag numArgs
    argPorts = take (2 * numArgs) $ argumentPorts dummyNode
    argList = fmap (makeArg args) (inputPort dummyNode : argPorts)

nestedPatternNodeToIcon :: String -> [Labeled (Maybe SgNamedNode)] -> Set.Set (NodeName, Edge) -> Icon
nestedPatternNodeToIcon str children args = NestedPatternApp
  (pure (Just (Named (NodeName (-1)) (TextBoxIcon str))))
  -- Why so many fmaps?
  ( (fmap . fmap . fmap . fmap) nodeToIcon children)
  asigendValueName
  where
    asigendValueName = makeArg args PatternValuePortConst


