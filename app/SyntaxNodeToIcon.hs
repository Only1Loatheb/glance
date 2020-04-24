module SyntaxNodeToIcon(
  lookupInEmbeddingMap
  , makeLNode
  , nodeToIcon
) where

import qualified Language.Haskell.Exts as Exts
import Control.Monad.State(State, state)
import Data.Either(partitionEithers)
import qualified Data.Graph.Inductive.Graph as ING
import qualified Data.Graph.Inductive.PatriciaTree as FGR
import Data.List(find)
import qualified Data.StringMap as SMap
import qualified Data.IntMap as IMap
import Data.Semigroup(Semigroup, (<>))
import qualified Data.Set as Set

import           PortConstants(
  inputPort
  , resultPort
  , argumentPorts
  , multiIfValuePorts
  , multiIfBoolPorts
  )
import           Types(
  Labeled(..)
  , Icon(..)
  , SyntaxNode(..)
  , Edge(..)
  , EdgeOption(..)
  , NameAndPort(..)
  , IDState
  , SgNamedNode
  , NodeName(..)
  , Port
  , LikeApplyFlavor(..)
  , CaseOrMultiIfTag(..)
  , IDState(..)
  , Embedder(..)
  , mkEmbedder
  , Named(..)
  , EmbedderSyntaxNode
  , AnnotatedGraph
  )
import Util(
  nameAndPort
  , makeSimpleEdge
  , justName
  , maybeBoolToBool
  , nodeNameToInt
  )
import SyntaxGraph(SyntaxGraph(..))
import SimpSyntaxToSyntaxGraph(
  translateStringToSyntaxGraph,
  customParseDecl
  )


nodeToIcon :: EmbedderSyntaxNode -> Icon
nodeToIcon (Embedder embeddedNodes node) = case node of
  (ApplyNode flavor x)
    -> nestedApplySyntaxNodeToIcon flavor x embeddedNodes
  (PatternApplyNode s children)
    -> nestedPatternNodeToIcon s children
  (NameNode s) -> TextBoxIcon s
  (BindNameNode s) -> BindTextBoxIcon s
  (LiteralNode s) -> TextBoxIcon s
  (FunctionDefNode labels str bodyNodes)
    -> nestedLambdaToIcon labels embeddedNodes str bodyNodes
  CaseResultNode -> CaseResultIcon
  (CaseOrMultiIfNode tag x)
    -> nestedCaseOrMultiIfNodeToIcon tag x embeddedNodes
  -- (ListCompNode) -> listCompIcon embeddedNodes -- TODO actualy embede nodes

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

nestedLambdaToIcon ::
  [String]  -- labels
  -> Set.Set (NodeName, Edge)  -- embedded icons
  -> String
  -> Set.Set NodeName  -- body nodes
  -> Icon
nestedLambdaToIcon labels embeddedNodes str =
  LambdaIcon labels (Labeled embeddedBodyNode str)
  where
    dummyNode = FunctionDefNode [] str Set.empty
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

nestedPatternNodeToIcon :: String -> [Labeled (Maybe SgNamedNode)] -> Icon
nestedPatternNodeToIcon str children = NestedPatternApp
  (pure (Just (Named (NodeName (-1)) (TextBoxIcon str))))
  -- Why so many fmaps?
  ( (fmap . fmap . fmap . fmap) nodeToIcon children)


-- Exported functions

makeLNode :: SgNamedNode -> ING.LNode SgNamedNode
makeLNode namedNode@(Named (NodeName name) _) = (name, namedNode)

lookupInEmbeddingMap :: NodeName -> IMap.IntMap NodeName -> NodeName
lookupInEmbeddingMap origName eMap = lookupHelper origName where
  lookupHelper :: NodeName -> NodeName
  lookupHelper name@(NodeName nameInt) = case IMap.lookup nameInt eMap of
    Nothing -> name
    Just parent -> if parent == origName
      then error $ "lookupInEmbeddingMap: Found cycle. Node = "
           ++ show origName ++ "\nEmbedding Map = " ++ show eMap
      else lookupHelper parent