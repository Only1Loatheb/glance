{-# LANGUAGE PatternSynonyms #-}

module SyntaxNodeToIcon(
  nodeToIcon
) where

import Data.List(find)
import qualified Data.Set as Set

import           PortConstants(
  inputPort
  , argumentPorts
  , pattern PatternUnpackingPort
  , listCompQualPorts
  )
import           Types(
  Icon(..)
  , DiagramIcon(..)
  , SyntaxNode(..)
  , SyntaxNodeCore(..)
  , Edge(..)
  , NameAndPort(..)
  , NodeName(..)
  , Port
  , Embedder(..)
  , Named(..)
  , EmbedderSyntaxNode
  )

-- | Helper for makeArg
findArg :: Port -> (NodeName, Edge) -> Bool
findArg currentPort
  (argName
  , Edge _ (NameAndPort fromName fromPort, NameAndPort toName toPort))
  | argName == fromName = currentPort == toPort
  | argName == toName = currentPort == fromPort
  | otherwise = False -- This case should never happen

makeArg :: Set.Set (NodeName, Edge) -> Port -> Maybe NodeName
makeArg embeddedNodes port = fst <$> find (findArg port) embeddedNodes

nodeToIcon :: EmbedderSyntaxNode -> Icon
nodeToIcon (Embedder embeddedNodes node@(SyntaxNode (ApplyNode flavor numArgs) src))
  = Icon (NestedApply flavor headIcon argList) src where
    argPorts = take numArgs (argumentPorts node)
    headIcon = makeArg embeddedNodes (inputPort node)
    argList = fmap (makeArg embeddedNodes) argPorts


nodeToIcon (Embedder embeddedNodes (SyntaxNode (PatternApplyNode str children) src))
  = Icon (
    NestedPatternApp
    icon
    -- Why so many fmaps?
    ( (fmap . fmap . fmap . fmap) nodeToIcon children)
    asigendValueName
  )
  src
  where
    icon = pure $ Just (Named (NodeName (-1)) (Icon (TextBoxIcon str) src))
    asigendValueName = makeArg embeddedNodes PatternUnpackingPort

nodeToIcon (Embedder _embeddedNodes (SyntaxNode (BindNameNode str) src))
  = Icon (BindTextBoxIcon str) src

nodeToIcon (Embedder _embeddedNodes (SyntaxNode (LiteralNode str) src))
  = Icon (TextBoxIcon str) src

nodeToIcon (Embedder _embeddedNodes (SyntaxNode (FunctionArgNode labels) src))
  = Icon (FunctionArgIcon labels) src 
  
nodeToIcon (Embedder embeddedNodes node@(SyntaxNode (FunctionValueNode str regionInfo) src))
  = Icon (FunctionDefIcon str regionInfo embeddedBodyNode) src where
    embeddedBodyNode = makeArg embeddedNodes (inputPort node)

nodeToIcon (Embedder _embeddedNodes (SyntaxNode CaseResultNode src))
  = Icon CaseResultIcon src

nodeToIcon (Embedder embeddedNodes node@(SyntaxNode (CaseNode flavor numArgs) src))
  = Icon (NestedCaseIcon flavor argList) src
  where
    argPorts = take (2 * numArgs) $ argumentPorts node
    argList = fmap (makeArg embeddedNodes) (inputPort node : argPorts)

nodeToIcon (Embedder embeddedNodes node@(SyntaxNode (ListCompNode genCount qualCount) src))
  = Icon icon src where

    genList = fmap (makeArg embeddedNodes) (take genCount $ argumentPorts node)

    qualList = fmap (makeArg embeddedNodes) (take qualCount listCompQualPorts)

    item = makeArg embeddedNodes (inputPort node)
    
    icon = ListCompIcon item genList qualList

nodeToIcon (Embedder embeddedNodes node@(SyntaxNode (ListLitNode flavor litCount delimiters) src))
  = Icon diagramIcon src where
    diagramIcon = ListLitIcon flavor maybeEmbeddedNodeNames delimiters
    maybeEmbeddedNodeNames = fmap (makeArg embeddedNodes) (take litCount $ argumentPorts node)

