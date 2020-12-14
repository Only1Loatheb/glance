{-# LANGUAGE PatternSynonyms #-}

module SyntaxNodeToIcon(
  nodeToIcon
) where

import Data.List(find)
import qualified Data.Set as Set

import           PortConstants(
  inputPort
  , argumentPortList
  , pattern PatternUnpackingPort
  , listCompQualPortList
  , caseValuePorts
  , caseCondPortList
  )
import           Types(
  Icon(..)
  , DiagramIcon(..)
  , SyntaxNode(..)
  , SyntaxNodeCore(..)
  , Edge(..)
  ,  Named(..)
  , NodeName(..)
  , Port
  , Embedder(..)
  , EmbedderSyntaxNode
  , Labeled(..)
  )

-- | Helper for makeArg
findArg :: Port -> (NodeName, Edge) -> Bool
findArg currentPort
  (argName
  , Edge _ (Named nameFrom fromPort, Named nameTo toPort))
  | argName == nameFrom = currentPort == toPort
  | argName == nameTo = currentPort == fromPort
  | otherwise = False -- This case should never happen

makeArg :: Set.Set (NodeName, Edge) -> Port -> Maybe NodeName
makeArg embeddedNodes port = fst <$> find (findArg port) embeddedNodes

nodeToIcon :: EmbedderSyntaxNode -> Icon
nodeToIcon (Embedder embeddedNodes node@(SyntaxNode (ApplyNode flavor numArgs) src))
  = Icon (NestedApply flavor headIcon argList) src where
    argPorts = take numArgs (argumentPortList node)
    headIcon = makeArg embeddedNodes (inputPort node)
    argList = fmap (makeArg embeddedNodes) argPorts


nodeToIcon (Embedder embeddedNodes node@(SyntaxNode (PatternNode str children) src))
  = Icon ( NestedPatternApp str patternList asigendValueName ) src where
    patternList = zipWith zipper (argumentPortList node) children
    asigendValueName = makeArg embeddedNodes PatternUnpackingPort
    zipper :: Port -> String -> Labeled (Maybe NodeName)
    zipper port patternLabel = Labeled patternLabel (makeArg embeddedNodes port)

nodeToIcon (Embedder _embeddedNodes (SyntaxNode (BindNameNode str) src))
  = Icon (BindTextBoxIcon str) src

nodeToIcon (Embedder _embeddedNodes (SyntaxNode (LiteralNode str) src))
  = Icon (TextBoxIcon str) src

nodeToIcon (Embedder _embeddedNodes (SyntaxNode (FunctionArgNode labels regionInfo) src))
  = Icon (FunctionArgIcon labels regionInfo) src 
  
nodeToIcon (Embedder embeddedNodes node@(SyntaxNode (FunctionValueNode str) src))
  = Icon (FunctionDefIcon str embeddedBodyNode) src where
    embeddedBodyNode = makeArg embeddedNodes (inputPort node)

nodeToIcon (Embedder _embeddedNodes (SyntaxNode CaseResultNode src))
  = Icon CaseResultIcon src

nodeToIcon (Embedder embeddedNodes node@(SyntaxNode (CaseNode flavor numArgs) src))
  = Icon (NestedCaseIcon flavor arg condsAndVals) src
  where
    argPorts = take  numArgs caseCondPortList
    valPorts = take  numArgs caseValuePorts
    conds = fmap (makeArg embeddedNodes) argPorts
    vals = fmap (makeArg embeddedNodes) valPorts
    condsAndVals = zip conds vals
    arg = makeArg embeddedNodes $ inputPort node

nodeToIcon (Embedder embeddedNodes node@(SyntaxNode (ListCompNode genCount qualCount) src))
  = Icon icon src where

    genList = fmap (makeArg embeddedNodes) (take genCount $ argumentPortList node)

    qualList = fmap (makeArg embeddedNodes) (take qualCount listCompQualPortList)

    item = makeArg embeddedNodes (inputPort node)
    
    icon = ListCompIcon item genList qualList

nodeToIcon (Embedder embeddedNodes node@(SyntaxNode (ListLitNode flavor litCount delimiters) src))
  = Icon diagramIcon src where
    diagramIcon = ListLitIcon flavor maybeEmbeddedNodeNames delimiters
    maybeEmbeddedNodeNames = fmap (makeArg embeddedNodes) (take litCount $ argumentPortList node)

