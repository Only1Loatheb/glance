{-# LANGUAGE NoMonomorphismRestriction, TupleSections, PatternSynonyms #-}

module SimpSyntaxToSyntaxGraph(
  translateStringToSyntaxGraph
  , translateDeclToSyntaxGraph
  , customParseDecl
  -- for tests 
  , makeBox
  , evalExp
  , evalDecl
) where


import Control.Monad(replicateM)
import Control.Monad.State(State, evalState)
import Data.List(unzip5, partition, intercalate)
import qualified Data.Set as Set
import qualified Data.Map as SMap
import qualified Data.IntMap as IMap
import           Data.Either                    ( partitionEithers )
import           Data.Maybe(fromMaybe,  
  catMaybes
  , mapMaybe
  , isNothing
  )
import qualified Language.Haskell.Exts as Exts
import qualified Language.Haskell.Exts.Pretty as PExts

import           PortConstants (
  inputPort
  , resultPort
  , argumentPortList
  , caseValuePorts
  , caseCondPortList
  , argPortList
  , caseCondPortList
  , valuePortList
  , pattern ResultPort
  , pattern InputPort
  , pattern PatternUnpackingPort
  , listCompQualPortList
  )

import HsSyntaxToSimpSyntax(
  SimpAlt(..)
  , stringToSimpDecl
  , SrcRef
  , SimpExp(..)
  , SimpExpCore(..)
  , SimpPat(..)
  , SimpPatCore(..)
  , SimpDecl(..)
  , SimpDeclCore(..)
  , SimpQStmt(..)
  , SimpQStmtCore(..)
  , customParseDecl
  , SelectorAndVal(..)
  , pattern FunctionCompositionStr
  , simpPatNameStr
  )
import           Types(
  ListLitFlavor
  , NameAndPort
  , IDState
  , SgNamedNode
  , SyntaxNode(SyntaxNode)
  , SyntaxNodeCore(..)
  , NodeName(..)
  , ApplyFlavor(..)
  , CaseFlavor(..)
  , Named(..)
  , mkEmbedder
  , Port(..)
  , Edge(..)
  , SgBindMap
  , Delimiters
  )
import Util(
  makeSimpleEdge
  , makeNotConstraintEdge
  , makeInvisibleEdge
  , nameAndPort
  )
import SyntaxGraph( 
    SyntaxGraph(..)
  , patternName
  , syntaxGraphFromNodes
  , syntaxGraphFromNodesEdges
  , graphAndRefToGraph
  , getUniqueName
  , getUniqueString
  , combineExpressions
  , namesInPattern
  , lookupReference
  , makeEdges
  , makeEdgesKeepBindings
  , asBindGraphZipper
  , GraphAndRef(..)
  , Reference
  , SgSink(..)
  , SgBind
  , grNamePortToGrRef
  , initialIdState
  , EvalContext
  , edgeFromPortToRef
  , edgeFromRefToPort
  , makeAsBindGraph
  , graphsToComponents
  , combineFromPortToGraph
  , deleteBindings
  , combineFromGraphToPort
  )
import StringSymbols(
  typeSignatureSeparatorStr
  , typeNameSeparatorStr
  , patternWildCardStr
  , unusedArgumentStr
  , defaultPatternNameStr
  , showLiteral
  , showSignlessLit
  , getFuncDefLabel
  )
  
import FuncDefRegionInfo(lambdaLevel, getFuncDefRegionInfo, FuncDefRegionInfo)
import Control.Arrow (Arrow(first))
import Diagrams.Prelude
  
-- OVERVIEW --
-- SimpSyntaxToSyntaxGraph SimpExp into subgraf of type SyntaxGraph
-- Generated SyntaxGraph has sgNodes sgEdges sgSinks sgBinds sgEmbedMap
-- for one SimpExp
-- sgNodes has unik ID called Name
-- ARCHITECTURE NOTE --
-- The core functions and data types used in this module are in SyntaxNodeToIcon.
-- The SyntaxNodeToIcon also contains most/all of the translation functions that
-- do not use Language.Haskell.Exts.

makeBox :: (String, SrcRef) -> State IDState (SyntaxGraph, NameAndPort)
makeBox (str, s) = do
  name <- getUniqueName
  let boxGraph = syntaxGraphFromNodes (Set.singleton (Named name (mkEmbedder (SyntaxNode (LiteralNode str) s))))
  pure (boxGraph, nameAndPort name ResultPort)

evalLit :: Exts.Literal l -> SrcRef -> State IDState (SyntaxGraph, NameAndPort)
evalLit lit s = makeBox (showSignlessLit lit, s)

-- evalExp :: Show l =>
--              EvalContext
--              -> SimpExp l
--              -> State IDState GraphAndRef
-- evalExp c x = deleteBindingsWithRef <$> evalExp' c x

evalExp :: EvalContext -> SimpExp -> State IDState GraphAndRef
evalExp c simpExp@(SimpExp l x) = case x of
  SeName name -> strToGraphRef c name l
  SeLit lit -> grNamePortToGrRef <$> evalLit lit l
  SeApp _ _ -> grNamePortToGrRef <$> evalApp c simpExp
  SeLambda argPatterns e functionName -> grNamePortToGrRef <$> evalLambda l c argPatterns e functionName
  SeLet decls expr -> evalLet c decls expr
  SeCase expr alts -> grNamePortToGrRef <$> evalCase c expr alts
  SeMultiIf selectorsAndVals -> grNamePortToGrRef <$> evalMultiIf c selectorsAndVals l
  SeListComp e eList -> evalListComp c l e eList
  SeListLit exps delimiters flavor -> grNamePortToGrRef <$> evalListLit c l exps delimiters flavor

-- BEGIN apply and compose helper functions

evalFunExpAndArgs ::
  EvalContext
  -> ApplyFlavor
  -> (SimpExp, [SimpExp])
  -> State IDState (SyntaxGraph, NameAndPort)
evalFunExpAndArgs c flavor (funExp@(SimpExp seSrcRef _), argExps) = do
  funVal <- evalExp c funExp
  argVals <- mapM (evalExp c) argExps
  applyIconName <- getUniqueName
  pure
    $ makeApplyGraph seSrcRef (length argExps) flavor False applyIconName funVal argVals

-- END apply and compose helper functions
evalFunctionComposition ::
  EvalContext -> SrcRef-> [SimpExp] -> State IDState (SyntaxGraph, NameAndPort)
evalFunctionComposition c seSrcRef functions = do
  let reversedFunctios = reverse functions
  evaluatedFunctions <- mapM (evalExp c) reversedFunctios
  neverUsedPort <- Left <$> getUniqueString unusedArgumentStr
  applyIconName <- getUniqueName
  pure $ makeApplyGraph
    seSrcRef
    (length evaluatedFunctions)
    ComposeFlavor
    False
    applyIconName
    (GraphAndRef mempty neverUsedPort)
    evaluatedFunctions

-- | Turn (a . b . c) into [a, b, c]
compositionToList :: SimpExp -> [SimpExp]
compositionToList e = case e of
  (SimpExp _ (SeApp 
    (SimpExp _  (SeApp 
      (SimpExp _  (SeName FunctionCompositionStr))
      f1))
    f2))
    -> f1 : compositionToList f2
  x -> [x]

-- BEGIN evaluateAppExpression

-- TODO add test for this function
-- | Given an App expression, return
-- (function, list of arguments)
appExpToFuncArgs :: SimpExp -> (SimpExp, [SimpExp])
appExpToFuncArgs e = case e of
  SimpExp _ (SeApp  exp1 exp2) -> (funExp, args <> [exp2])
    where
      (funExp, args) = appExpToFuncArgs exp1
  x -> (x, [])

-- TODO Refactor this and all sub-expressions
evalApp ::
  EvalContext -> SimpExp
  -> State IDState (SyntaxGraph, NameAndPort)
evalApp c expr = case expr of
  -- TODO This pattern for "." appears at least twice in this file. Refactor?
  (SimpExp l (SeApp 
    (SimpExp _  (SeApp 
      (SimpExp _  (SeName FunctionCompositionStr))
      _))
    _))
    -> evalFunctionComposition c l (compositionToList expr)
  _ -> evalFunExpAndArgs c ApplyFlavor (appExpToFuncArgs expr)
    
-- END evaluateAppExpression

-- BEGIN evalGeneralLet

getBoundVarName :: SimpDecl -> EvalContext
getBoundVarName (SimpDecl _s d) = case d of
  SdPatBind pat _ -> namesInPattern
                     -- TODO Should evalState be used here?
                     $ evalState (evalPattern pat) initialIdState
  SdTypeSig {} -> Set.empty
  SdCatchAll {} -> Set.empty

-- TODO make it rerurn -> State IDState [(SyntaxGraph, EvalContext)]
evalDecls ::
  EvalContext -> [SimpDecl] -> State IDState (SyntaxGraph, EvalContext)
evalDecls c decls = do
  let
    boundNames = Set.unions (fmap getBoundVarName decls)
    augmentedContext = Set.union boundNames c
  mdeclGraphAndRefs <- mapM (evalDecl augmentedContext) decls
  let 
    declGraphAndRefs = catMaybes mdeclGraphAndRefs
    declGraphs = map graphAndRefToGraph declGraphAndRefs
    declGraph = mconcat declGraphs
  pure (declGraph, augmentedContext)

evalLet ::
  EvalContext
  -> [SimpDecl]
  -> SimpExp
  -> State IDState GraphAndRef
evalLet c decls expr = do
  (bindGraph, bindContext) <- evalDecls c decls
  expVal <- evalExp bindContext expr
  let
    GraphAndRef expGraph expResult = expVal
    newGraph = makeEdges makeSimpleEdge $ expGraph <> bindGraph
    bindings = sgBinds bindGraph
  pure $ GraphAndRef newGraph (lookupReference bindings expResult)

-- END evalGeneralLet

evalSelectorAndVal ::
  EvalContext -> SelectorAndVal -> State IDState (SrcRef, GraphAndRef, GraphAndRef)
evalSelectorAndVal c (SelectorAndVal s sel val)
  = do
  selGraphAndRef <- evalExp c sel
  valGraphAndRef <- evalExp c val
  pure (s, selGraphAndRef , valGraphAndRef) 

evalMultiIf ::
  EvalContext -> [SelectorAndVal] -> SrcRef -> State IDState (SyntaxGraph, NameAndPort)
evalMultiIf c selectorsAndVals seSrcRef = do
  selAndValExps <- mapM (evalSelectorAndVal c) selectorsAndVals
  multiIfName <- getUniqueName
  let 
    (_srcRefs, selGraphAndRefs, valGraphAndRefs) = unzip3 selAndValExps
    graphAndNamedPort = makeMultiIfGraph (length selectorsAndVals) seSrcRef selGraphAndRefs valGraphAndRefs multiIfName
  pure graphAndNamedPort
  

makeMultiIfGraph ::
  Int
  -> SrcRef
  -> [GraphAndRef]
  -> [GraphAndRef]
  -> NodeName
  -> (SyntaxGraph, NameAndPort)
makeMultiIfGraph  numPairs srcRefs bools exps multiIfName
  = (newGraph, nameAndPort multiIfName (resultPort multiIfNode))
  where
    multiIfNode = SyntaxNode (CaseNode MultiIfFlavor numPairs) srcRefs
    expsWithPorts = zip exps $ map (nameAndPort multiIfName) caseValuePorts
    boolsWithPorts = zip bools $ map (nameAndPort multiIfName) caseCondPortList
    combindedGraph = combineExpressions False $ expsWithPorts <> boolsWithPorts
    icons = [Named multiIfName (mkEmbedder multiIfNode)]
    newGraph = syntaxGraphFromNodes (Set.fromList icons) <> combindedGraph

-- BEGIN BEGIN BEGIN BEGIN BEGIN evalCase

-- TODO patRhsAreConnected is sometimes incorrectly true if the pat is just a
-- name
-- returns (combined graph, pattern reference, rhs reference)
evalAlt ::
  EvalContext
  -> SimpAlt
  -> State IDState (Bool, SyntaxGraph, Reference, Reference, Maybe String)
evalAlt c (SimpAlt _s pat rhs) = do
  ((GraphAndRef patGraph patRef, mPatAsName), GraphAndRef rhsGraph rhsRef) <-
    makePatternGraph c pat rhs
  let
    grWithEdges = makeEdgesKeepBindings makeNotConstraintEdge (rhsGraph <> patGraph)
    lookedUpRhsRef = lookupReference (sgBinds grWithEdges) rhsRef
    -- The pattern and rhs are conneted if makeEdges added extra edges, or if
    -- the rhsRef refers to a source in the pattern.
    patRhsAreConnected
      = rhsRef /= lookedUpRhsRef
        -- \|| ( length (sgEdges grWithEdges) > (length (sgEdges rhsGraph) + length (sgEdges patGraph)))
  pure (patRhsAreConnected
       , grWithEdges
       , makePatternRefForUnpacking patRef
       , lookedUpRhsRef
       , mPatAsName)

evalCase ::
  EvalContext -> SimpExp -> [SimpAlt]
  -> State IDState (SyntaxGraph, NameAndPort)
evalCase c simpExp@(SimpExp seSrcRef _) alts =
  let
    numAlts = length alts
    altSrcRefs = map simpAltRef alts
  in
    evalCaseHelper (length alts) c seSrcRef altSrcRefs
    <$>
    getUniqueName
    <*>
    replicateM numAlts getUniqueName
    <*>
    evalExp c simpExp
    <*>
    mapM (evalAlt c) alts

evalCaseHelper ::
  Int
  -> EvalContext
  -> SrcRef
  -> [SrcRef]
  -> NodeName
  -> [NodeName]
  -> GraphAndRef
  -> [(Bool, SyntaxGraph, Reference, Reference, Maybe String)]
  -> (SyntaxGraph, NameAndPort)
evalCaseHelper numAlts context seSrcRef altsSrcRefs caseIconName resultIconNames (GraphAndRef expGraph expRef) evaledAlts
  = result
  where
    (patRhsConnected, altGraphs, unpackingPatRef, rhsRefs, asNames) = unzip5 evaledAlts
    combindedAltGraph = mconcat altGraphs
    caseNode = SyntaxNode (CaseNode CaseFlavor numAlts) seSrcRef
    caseNodeGraph = makeCaseNodeGraph caseIconName caseNode expRef

    bindGraph = makeAsBindGraph expRef asNames
    conditionEdgesGraph = makeConditionEdges unpackingPatRef caseIconName 
    caseResultGraphs = makeRhsGraph patRhsConnected rhsRefs caseIconName resultIconNames altsSrcRefs

    finalGraph = makeEdges makeSimpleEdge $ mconcat [
      bindGraph
      , conditionEdgesGraph
      , caseResultGraphs
      , expGraph
      , caseNodeGraph
      , combindedAltGraph
      ]
    result = (finalGraph, nameAndPort caseIconName (resultPort caseNode))

makeCaseNodeGraph :: NodeName -> SyntaxNode -> Reference -> SyntaxGraph
makeCaseNodeGraph caseIconName caseNode expRef = caseGraph where
    icons = Set.singleton (Named caseIconName (mkEmbedder caseNode))
    caseNodeNameAndPort = nameAndPort caseIconName (inputPort caseNode)
    inputEdgeGraph = edgeFromRefToPort makeSimpleEdge expRef caseNodeNameAndPort
    caseGraph = inputEdgeGraph <> syntaxGraphFromNodes icons

makeConditionEdges :: [Reference] -> NodeName -> SyntaxGraph
makeConditionEdges unpackingPatRef caseIconName = conditionEdgesGraph where
    caseConditionNamedPorts = map (nameAndPort caseIconName) caseCondPortList
    patEdgesGraphs = zipWith (edgeFromPortToRef makeNotConstraintEdge) unpackingPatRef caseConditionNamedPorts
    conditionEdgesGraph = mconcat patEdgesGraphs

makeRhsGraph :: [Bool] -> [Reference ] -> NodeName -> [NodeName] -> [SrcRef]->  SyntaxGraph
makeRhsGraph patRhsConnected rhsRefs caseIconName resultIconNames altsSrcRefs = caseResultGraphs where
  caseValueNamedPorts = map (nameAndPort caseIconName) caseValuePorts
  rhsRefNamedPortAndSrc = zip3 rhsRefs caseValueNamedPorts altsSrcRefs
  isConnectedAndValueRefAndNamedPorts =  zip patRhsConnected rhsRefNamedPortAndSrc

  --TODO partitionEithers
  (connected, unConnected) = partition fst isConnectedAndValueRefAndNamedPorts
  unConnectedRhss = map (\(_,(x,y,z))->(x,y)) unConnected
  connectedRhss = map snd connected

  caseEdgeGraph = mconcat $ fmap (uncurry (edgeFromRefToPort makeSimpleEdge)) unConnectedRhss

  resultNodeGraph = mconcat $ zipWith makeCaseResult resultIconNames connectedRhss

  caseResultGraphs = caseEdgeGraph <> resultNodeGraph

makeCaseResult :: NodeName -> (Reference, NameAndPort, SrcRef) -> SyntaxGraph
makeCaseResult resultIconName (rhsRef, caseValueNamedPort, seSrcRef) = case rhsRef of
  Left _ -> mempty
  Right rhsPort -> syntaxGraphFromNodesEdges rhsNewIcons rhsNewEdges
    where
      rhsNewIcons = Set.singleton (Named resultIconName (mkEmbedder $ SyntaxNode CaseResultNode seSrcRef))
      rhsNewEdges = Set.fromList [
        makeSimpleEdge (rhsPort, nameAndPort resultIconName ResultPort)
        , makeNotConstraintEdge ( nameAndPort resultIconName (Port 1), caseValueNamedPort)
        ]
-- END END END END END evalCase

-- BEGIN BEGIN BEGIN BEGIN BEGIN evalLambda 
evalLambda ::
  SrcRef
  -> EvalContext
  -> [SimpPat]
  -> SimpExp
  -> Maybe String
  -> State IDState (SyntaxGraph, NameAndPort)
evalLambda seSrcRef context argPatterns expr functionName = do
  lambdaName <- getUniqueName
  argNodeName <- getUniqueName
  argPatternValsWithAsNames <- mapM evalPattern argPatterns
  let
    lambdaLabel = getFuncDefLabel lambdaName functionName
    argPatternStrings = Set.unions $ fmap namesInPattern argPatternValsWithAsNames
    rhsContext = Set.unions [argPatternStrings, context, Set.singleton lambdaLabel]
  GraphAndRef rhsRawGraph rhsRef <- evalExp rhsContext expr
  let
    argPatternVals = fmap fst argPatternValsWithAsNames
    argPatternGraph = mconcat $ fmap graphAndRefToGraph argPatternVals
    nodesGraph = argPatternGraph <> rhsRawGraph
    (outputReference,isOutputStraightFromInput) 
      = getOutputNameAndPort rhsRef  argPatternVals lambdaPorts  nodesGraph
  
    argNode = makeLambdaArgumentNode argPatternValsWithAsNames regionInfo seSrcRef 
    regionInfo = getFuncDefRegionInfo graphWithoutRegionNode
    lambdaNode = makeLambdaNode lambdaLabel  seSrcRef
    lambdaPorts = map (nameAndPort argNodeName) $ argumentPortList lambdaNode
    lambdaValueLink =  makeEitherEdgeOrSgBind outputReference (nameAndPort lambdaName (inputPort lambdaNode))
  -- (valueGraph,outputNameAndPort) <- getValueGraphAndNamedPort outputReference

    -- lambdaValueEdge = makeSimpleEdge (outputNameAndPort,)
    -- TODO move adding drawing rank edge after graph simplification and collapsing
    constraintEdgeList = constraintLambdaArgAboveValue outputReference argNodeName lambdaName regionInfo

    (argPatternEdges', newBinds') =
      partitionEithers $ zipWith makePatternEdgeInLambda argPatternVals lambdaPorts
    lambdaEdges = constraintEdgeList ++ argPatternEdges'

    lambdaArgGraph = makeLambdaArgGraph lambdaEdges newBinds' (argNodeName,argNode)
    lambdaIconAndOutputGraph = makeLambdaOutputGraph (lambdaName ,lambdaNode) [lambdaValueLink] lambdaLabel

    asBindGraph = mconcat $ zipWith asBindGraphZipper (fmap snd argPatternValsWithAsNames) lambdaPorts

    combinedGraph = makeEdgesKeepBindings makeSimpleEdge (rhsRawGraph <> argPatternGraph  <> asBindGraph )
    graphWithoutRegionNode = makeEdgesKeepBindings makeNotConstraintEdge (lambdaIconAndOutputGraph <> combinedGraph)
    finalGraph = makeEdges makeSimpleEdge (graphWithoutRegionNode <> lambdaArgGraph)

    resultNameAndPort = nameAndPort lambdaName (resultPort lambdaNode)
  if isIdLambda isOutputStraightFromInput argPatterns functionName
  then makeBox ( Set.elemAt 0 argPatternStrings, seSrcRef)
  else pure (finalGraph, resultNameAndPort)

constraintLambdaArgAboveValue :: Reference -> NodeName -> NodeName -> FuncDefRegionInfo-> [Edge]
constraintLambdaArgAboveValue outputReference argNodeName lambdaName regionInfo = -- case outputReference of 
    [makeInvisibleEdge len (nameAndPort argNodeName ResultPort,
      nameAndPort lambdaName InputPort)] where
      len = 1 + lambdaLevel regionInfo
    

isIdLambda :: Bool -> [SimpPat] -> Maybe String -> Bool
isIdLambda isOutputStraightFromInput argPatterns functionName
  = isOutputStraightFromInput && length argPatterns == 1 && isNothing functionName 

getOutputNameAndPort :: Reference
  -> [GraphAndRef]
  -> [NameAndPort]
  -> SyntaxGraph
  -> (Reference, Bool)
getOutputNameAndPort (Right np)        _              _           _            = refAndisItoO where
  refAndisItoO = (Right np, False)
getOutputNameAndPort strRef@(Left str) argPatternVals lambdaPorts allNodeGraps = refAndisItoO where
  refAndisItoO = (maybeNamedPort, isItoO) 
  argumentReferences = catMaybes $ zipWith (makeReferenceToArgument strRef) argPatternVals lambdaPorts
  referenceToExpresion = lookupReference (sgBinds allNodeGraps) strRef
  maybeNamedPort = getOutputNameAndPort' argumentReferences referenceToExpresion str
  isItoO = not $ null argumentReferences

getOutputNameAndPort' :: [NameAndPort] -> Reference -> String -> Reference  
getOutputNameAndPort'  (np:_) _ _= Right np
getOutputNameAndPort'  _ (Right np) _= Right np
getOutputNameAndPort'  _ _ str = Left str

makeReferenceToArgument :: Reference -> GraphAndRef -> NameAndPort  -> Maybe NameAndPort
makeReferenceToArgument rhsRef (GraphAndRef _ ref) lamPort = if rhsRef == ref
      then Just  lamPort
      else Nothing

makeLambdaNode :: String  -> SrcRef -> SyntaxNode
makeLambdaNode  functionName seSrcRef  = node where
  node = SyntaxNode (FunctionValueNode functionName) seSrcRef

makeLambdaArgumentNode :: [(GraphAndRef, Maybe String)] -> FuncDefRegionInfo -> SrcRef -> SyntaxNode
makeLambdaArgumentNode argPatternValsWithAsNames regionInfo seSrcRef = node where 
  paramNames = fmap patternName argPatternValsWithAsNames
  node = SyntaxNode (FunctionArgNode paramNames regionInfo) seSrcRef


makeLambdaOutputGraph :: (NodeName, SyntaxNode)
  -> [Either Edge SgBind]
  -> String
  -> SyntaxGraph
makeLambdaOutputGraph  (lambdaName ,lambdaNode) lambdaValueLink functionName = graph where
  (valueEdges', valueBinds') = partitionEithers lambdaValueLink
  valueEdges = Set.fromList valueEdges'
  bindForRecursion = (functionName, Right $ nameAndPort lambdaName ResultPort)
  valueBinds = SMap.fromList (bindForRecursion : valueBinds')
  lambdaIconSet = Set.singleton (Named lambdaName (mkEmbedder lambdaNode))
  graph = SyntaxGraph lambdaIconSet valueEdges mempty valueBinds mempty


makeLambdaArgGraph :: [Edge]
                        -> [SgBind] -> (NodeName, SyntaxNode) -> SyntaxGraph
makeLambdaArgGraph argPatternEdgesList binds (argNodeName, argNode) = graph where
  bindsSet = SMap.fromList binds
  argPatternEdges = Set.fromList argPatternEdgesList
  lambdaIconSet = Set.singleton (Named argNodeName (mkEmbedder argNode))
  graph = SyntaxGraph lambdaIconSet argPatternEdges mempty bindsSet mempty

-- lambda
makeEitherEdgeOrSgBind :: Reference -> NameAndPort -> Either Edge SgBind
makeEitherEdgeOrSgBind ref lamPort = case ref of
  Right np -> Left $ makeSimpleEdge (np, lamPort)
  Left str -> Right (str, Right lamPort)

makePatternEdgeInLambda ::GraphAndRef -> NameAndPort -> Either Edge SgBind
makePatternEdgeInLambda (GraphAndRef _ ref) lamPort = case ref of
  Right (Named name _) 
    -> Left $ makeSimpleEdge (lamPort, patternValueInputPort name )
  Left str -> Right (str, Right lamPort)

patternValueInputPort :: NodeName -> NameAndPort
patternValueInputPort name = Named name  PatternUnpackingPort
-- END END END END END evalLambda 

makeApplyGraph ::
  SrcRef
  -> Int
  -> ApplyFlavor
  -> Bool
  -> NodeName
  -> GraphAndRef
  -> [GraphAndRef]
  -> (SyntaxGraph, NameAndPort)
makeApplyGraph srcRef numArgs applyFlavor inPattern applyIconName funVal argVals
  = (newGraph <> combinedGraph
    , nameAndPort applyIconName (resultPort applyNode)
    )
  where
    applyNode = SyntaxNode (ApplyNode applyFlavor numArgs) srcRef
    argumentNamePorts
      = map (nameAndPort applyIconName) (argumentPortList applyNode)
    functionPort = nameAndPort applyIconName (inputPort applyNode)
    combinedGraph = combineExpressions inPattern
                    $ zip (funVal:argVals) (functionPort:argumentNamePorts)
    icons = [Named applyIconName (mkEmbedder applyNode)]
    newGraph = syntaxGraphFromNodes $ Set.fromList icons

-- BEGIN BEGIN BEGIN BEGIN BEGIN evalPattern
makePatternGraph ::
  EvalContext
  -> SimpPat
  -> SimpExp
  -> State IDState ((GraphAndRef, Maybe String), GraphAndRef)
makePatternGraph c pat e = do
  patGraphAndRef <- evalPattern pat
  let
    rhsContext = Set.union (namesInPattern patGraphAndRef) c
  rhsGraphAndRef <- evalExp rhsContext e
  pure (patGraphAndRef, rhsGraphAndRef)
-- TODO use in listComp
evalPatternApp ::
  String
  -> [SimpPat]
  -> SrcRef
  -> State IDState (SyntaxGraph, NameAndPort)
evalPatternApp constructorName patterns s = case patterns of
  [] -> makeBox (constructorName, s)
  _ ->  do
    patName <- getUniqueName
    evaledPatterns <- mapM evalPattern patterns
    pure $ makeNestedPatternGraph patName constructorName evaledPatterns s

evalPatternLit ::
  Exts.Sign Exts.SrcSpanInfo -> Exts.Literal Exts.SrcSpanInfo -> SrcRef -> State IDState (SyntaxGraph, NameAndPort)
evalPatternLit sign lit srcRef = makeBox (showLiteral sign lit, srcRef)
-- END evalPatternLit

evalPAsPat ::
  String -> SimpPat -> State IDState (GraphAndRef, Maybe String)
evalPAsPat outerName p = do
  (GraphAndRef evaledPatGraph evaledPatRef, mInnerName) <- evalPattern p
  let
    asBindGraph = makeAsBindGraph (Left outerName) [mInnerName]
  pure (GraphAndRef (asBindGraph <> evaledPatGraph) evaledPatRef
       , Just outerName)

-- strToGraphRef is not in SyntaxNodeToIcon, since it is only used by evalQName.
strToGraphRef :: EvalContext -> String -> SrcRef -> State IDState GraphAndRef
strToGraphRef c str srcRef = fmap mapper (makeBox (str,srcRef)) where
  mapper gr = if Set.member str c
    then GraphAndRef mempty (Left str)
    else grNamePortToGrRef gr

makePatternResult :: Functor f =>
  f (SyntaxGraph, NameAndPort) -> f (GraphAndRef, Maybe String)
makePatternResult
  = fmap (\(graph, namePort) -> (GraphAndRef graph (Right namePort), Nothing))

--TODO  IMPORTANT return also PatternUnpackingPort in all 
evalPattern :: SimpPat -> State IDState (GraphAndRef, Maybe String)
evalPattern (SimpPat s p) = case p of
  SpVar {} -> pure (GraphAndRef mempty (Left $ simpPatNameStr p ), Nothing)
  SpLit sign lit -> makePatternResult $ evalPatternLit sign lit s
  SpApp _ patterns -> makePatternResult $ evalPatternApp (simpPatNameStr p) patterns s
  SpAsPat _ pat -> evalPAsPat (simpPatNameStr p) pat
  SpWildCard -> makePatternResult $ makeBox (patternWildCardStr, s)
  -- _ -> error ("evalPattern todo: " <> show p)

makeNestedPatternGraph ::
  NodeName
  -> String
  -> [(GraphAndRef, Maybe String)]
  -> SrcRef
  -> (SyntaxGraph, NameAndPort)
makeNestedPatternGraph applyIconName funStr argVals srcRef = nestedApplyResult where
  dummyNode = SyntaxNode (PatternNode defaultPatternNameStr []) srcRef

  valueLabels = fmap patternLabels argVals

  graphsAndUnpackingRefrences = map (\(GraphAndRef g r,_) -> GraphAndRef g $ makePatternRefForUnpacking r)  argVals -- should be unpacking Port only for Patterns
  
  patternValuePorts = map (nameAndPort applyIconName) $ argumentPortList dummyNode
  combinedGraph = zipWith (combineFromPortToGraph makeNotConstraintEdge) graphsAndUnpackingRefrences  patternValuePorts

  patternNode = SyntaxNode (PatternNode funStr valueLabels) srcRef
  patternNodeGraph = syntaxGraphFromNodes $ Set.singleton (Named applyIconName (mkEmbedder patternNode))

  asNameBinds :: [SyntaxGraph]
  asNameBinds = map ((flip makeAsBindGraph) (map snd argVals)) (map (grRef. fst) argVals)

  graph = mconcat $ patternNodeGraph : asNameBinds ++ combinedGraph
   
  nestedApplyResult = ( makeEdgesKeepBindings makeSimpleEdge graph,  nameAndPort applyIconName (resultPort patternNode))

patternLabels :: (GraphAndRef, Maybe String) -> String
patternLabels asGraphAndRef@(_, providedLabel) = str where
  str = fromMaybe patName providedLabel
  patName = patternName asGraphAndRef
                    
-- END END END END END evalPattern
evalListLit :: EvalContext -> SrcRef -> [SimpExp] -> Delimiters -> ListLitFlavor -> State IDState (SyntaxGraph, NameAndPort)
evalListLit c l eList delimiters flavor = do
  listGenName <- getUniqueName
  graphsAndRefs <- mapM (evalExp c) eList
  let 
    listGenNode = SyntaxNode (ListLitNode flavor (length eList) delimiters) l 
    edgesGraph = makeListLitEdges listGenName graphsAndRefs (argumentPortList listGenNode)
    resultNameAndPort = nameAndPort listGenName (resultPort listGenNode)
    listGenNodeGraph = syntaxGraphFromNodes (Set.singleton (Named listGenName (mkEmbedder listGenNode)))
    finalGraph = makeEdges makeSimpleEdge $ listGenNodeGraph <> edgesGraph <> mconcat (map grGraph graphsAndRefs)
  pure (finalGraph, resultNameAndPort)

makeListLitEdges :: NodeName -> [GraphAndRef] -> [Port] -> SyntaxGraph
makeListLitEdges listGenName graphsAndRefs ports = mconcat edges where
  edges = zipWith (edgeFromRefToPort makeSimpleEdge) (map grRef graphsAndRefs) (map (nameAndPort listGenName) ports)

-- BEGIN BEGIN BEGIN BEGIN BEGIN list comp
-- valus form ListCompNode are connected to item constructor
-- TODO reconsider PORT architecture choise to identfy arguments
-- TODO improve connection to guard expresion
-- TODO connect valus from generators to ListCompNode 
evalListComp ::
  EvalContext -> SrcRef -> SimpExp -> [SimpQStmt] -> State IDState GraphAndRef
evalListComp context l  itemExp qualExps =  do  
  
  let decls = [d | (SimpQStmt _ (SqLet d )) <- qualExps]
  declGraphAndRefdeclContexts <-  mapM (evalDecls context) decls -- TODO add decls to context and graph
  let (declGraphAndRef, declContexts) = unzip declGraphAndRefdeclContexts
  let declContext =  Set.unions (context : declContexts)
  

  let gens  = [(srcRef, x) | (SimpQStmt srcRef x@SqGen {}) <- qualExps]
  genGRContextsAndGRpatRef <- mapM (evalSqGen declContext)  gens
  let genGraphsAndRefs =  fmap snd genGRContextsAndGRpatRef
  let genContext = Set.unions (declContext : map (namesInPattern . fst) genGRContextsAndGRpatRef)

  listCompItemGraphAndRef@(GraphAndRef _ listCompItemRef)  <- evalExp genContext itemExp
  let declGraphsAndRefs = fmap (getRefForListCompItem listCompItemRef) declGraphAndRef

  let quals = [q | (SimpQStmt _ (SqQual q)) <- qualExps]
  qualGraphsAndRefs <- mapM (evalExp genContext) quals

  listCompName <- getUniqueName

  let combinedGraphAndRef = makeListCompGraph context listCompName l listCompItemGraphAndRef qualGraphsAndRefs declGraphsAndRefs genGraphsAndRefs

  pure combinedGraphAndRef

getRefForListCompItem :: Reference -> SyntaxGraph -> GraphAndRef
getRefForListCompItem expResultRef bindGraph = GraphAndRef bindGraph ref where
  bindings = sgBinds bindGraph
  ref = lookupReference bindings expResultRef

-- evalSqGen :: EvalContext
--                -> SimpQStmt l
--                -> (State IDState GraphAndRef, Maybe String)
evalSqGen :: EvalContext -> (a, SimpQStmtCore) -> State IDState ((GraphAndRef, Maybe String), GraphInPatternRef)
evalSqGen context (_l, SqGen values itemPat) = do 
  (GraphAndRef patternGraph patternRef, itemContext) <- evalPattern itemPat
  (GraphAndRef valueGraph valueRef) <- evalExp context values -- TODO use "pat" to put value into item constructor
  let 
    graph = valueGraph <> patternGraph
    patternRefForUnpacking = makePatternRefForUnpacking patternRef
  pure ((GraphAndRef graph patternRef, itemContext),GraphInPatternRef graph valueRef patternRefForUnpacking)
evalSqGen _ _ = error "SimpQStmt must be SqGen" 

makePatternRefForUnpacking :: Reference -> Reference
makePatternRefForUnpacking (Right (Named name ResultPort)) = Right (Named name PatternUnpackingPort)
makePatternRefForUnpacking ref = ref

makeListCompGraph :: EvalContext -> NodeName -> SrcRef -> GraphAndRef
  -> [GraphAndRef] -> [GraphAndRef] -> [GraphInPatternRef] -> GraphAndRef
makeListCompGraph _context listCompName listCompSrcRef listCompItemGraphAndRef
  qualGraphsAndRefs declGraphsAndRefs genGraphsAndRefs 
  = GraphAndRef combinedGraph (Right listCompNodeRef) where

  (listCompNode, listCompNodeRef, listCompNodeGraph) 
    = makeListCompNodeGraph listCompName listCompSrcRef (length genGraphsAndRefs) (length qualGraphsAndRefs)

  listCompItemGraph = combineFromGraphToPort makeSimpleEdge
    listCompItemGraphAndRef ( Named listCompName  (inputPort listCompNode))

  qStmtGraph = makeQstmtGraph qualGraphsAndRefs declGraphsAndRefs genGraphsAndRefs 

  qualEdgeGraph = makeListCompQualEdgeGraph listCompName qualGraphsAndRefs

  listCompGenEdgeGraph = makeListCompGenEdgeGraph listCompName genGraphsAndRefs

  combinedGraph = makeEdges makeSimpleEdge
    (listCompItemGraph  <> listCompNodeGraph <> qStmtGraph <> qualEdgeGraph <> listCompGenEdgeGraph) 

makeListCompQualEdgeGraph :: NodeName -> [GraphAndRef] -> SyntaxGraph
makeListCompQualEdgeGraph listCompName qualGraphsAndRefs = qualEdgeGraph where 
  listCompQualNamedPorts = map (nameAndPort listCompName) listCompQualPortList
  qualEdgeGraph = mconcat $ zipWith (combineFromGraphToPort makeSimpleEdge) qualGraphsAndRefs listCompQualNamedPorts

makeListCompGenEdgeGraph :: NodeName -> [GraphInPatternRef] -> SyntaxGraph
makeListCompGenEdgeGraph listCompName genGraphsAndRefs = inEdgeGraph <> outEdgeGraph where
  listCompInNamedPorts = map (nameAndPort listCompName) argPortList
  listCompOutNamedPorts = map (nameAndPort listCompName) valuePortList

  graphAndValueRef = map graphInPatternRefToGraphAndRef genGraphsAndRefs
  graphAndPatternRef = map graphInPatternRefToGraphAndPat genGraphsAndRefs

  inEdgeGraph = mconcat $ zipWith (combineFromGraphToPort makeSimpleEdge) graphAndValueRef listCompInNamedPorts
  outEdgeGraph = mconcat $ zipWith (combineFromPortToGraph makeNotConstraintEdge) graphAndPatternRef listCompOutNamedPorts


makeListCompNodeGraph :: NodeName -> Exts.SrcSpan -> Int -> Int -> (SyntaxNode, NameAndPort, SyntaxGraph)
makeListCompNodeGraph listCompName listCompSrcRef genCount qualCount = (listCompNode, listCompNodeRef, listCompNodeGraph) where
  listCompNode = SyntaxNode (ListCompNode genCount qualCount) listCompSrcRef
  listCompNodeRef = nameAndPort listCompName (resultPort listCompNode)
  listCompNodeGraph = syntaxGraphFromNodes
    $ Set.singleton (Named listCompName (mkEmbedder listCompNode))

makeQstmtGraph :: [GraphAndRef] -> [GraphAndRef] -> [GraphInPatternRef] -> SyntaxGraph
makeQstmtGraph qualGraphsAndRefs declGraphsAndRefs genGraphsAndRefs = genGraphs <> qualGraphs where
  qualGraphs = mconcat $ fmap  graphAndRefToGraph (qualGraphsAndRefs ++ declGraphsAndRefs)
  genGraphs = mconcat $ fmap  graphInPatternRefToGraph genGraphsAndRefs

data GraphInPatternRef = GraphInPatternRef{
    _syntaxGraph :: SyntaxGraph
  , _valueReference :: Reference
  , _inPatternRef :: Reference
  }

graphInPatternRefToGraph :: GraphInPatternRef -> SyntaxGraph
graphInPatternRefToGraph (GraphInPatternRef g _ _) = g

graphInPatternRefToGraphAndRef :: GraphInPatternRef -> GraphAndRef
graphInPatternRefToGraphAndRef (GraphInPatternRef g r _) = GraphAndRef g r

graphInPatternRefToGraphAndPat :: GraphInPatternRef -> GraphAndRef
graphInPatternRefToGraphAndPat (GraphInPatternRef g _ p) = GraphAndRef g p
-- END END END END END List Comp 

-- TODO refactor with similar pattern functions
evalPatBind ::
  SrcRef -> EvalContext -> SimpPat -> SimpExp -> State IDState GraphAndRef
evalPatBind _ c pat e = do
  ((GraphAndRef patGraph patRef, mPatAsName), GraphAndRef rhsGraph rhsRef) <-
    makePatternGraph c pat e
  let
    (newEdges, newSinks, bindings) = evalPatBindHelper patRef rhsRef
    asBindGraph = makeAsBindGraph rhsRef [mPatAsName]
    gr = asBindGraph <> SyntaxGraph mempty newEdges newSinks bindings mempty
    combinedGraph = gr <> rhsGraph <> patGraph
  pure (GraphAndRef (makeEdgesKeepBindings makeSimpleEdge combinedGraph) patRef)

-- TODO refactor with similar pattern functions
evalPatBindHelper :: Reference -> Reference -> (Set.Set Edge, Set.Set SgSink, SgBindMap)
evalPatBindHelper patRef rhsRef = case patRef of
  (Left s) -> (mempty, mempty, SMap.singleton s rhsRef)
  (Right (Named name _)) -> 
    let patternValuePort = patternValueInputPort name in
      case rhsRef of
      (Left rhsStr) -> (mempty, Set.singleton (SgSink rhsStr patternValuePort), SMap.empty)
      (Right rhsPort) -> (Set.singleton (makeSimpleEdge (rhsPort, patternValuePort)), mempty, SMap.empty)

-- Pretty printing the entire type sig results in extra whitespace in the middle
evalTypeSig :: [Exts.Name Exts.SrcSpanInfo] -> Exts.Type Exts.SrcSpanInfo -> SrcRef
  -> State IDState (SyntaxGraph, NameAndPort)
evalTypeSig names typeForNames srcRef = makeBox
  (intercalate typeNameSeparatorStr (fmap prettyPrintWithoutNewlines names)
   ++ typeSignatureSeparatorStr
   ++ prettyPrintWithoutNewlines typeForNames
   , srcRef)
  where
    -- TODO Make custom version of prettyPrint for type signitures.
    -- Use (unwords . words) to convert consecutive whitspace characters to one
    -- space.
    prettyPrintWithoutNewlines = unwords . words . Exts.prettyPrint

evalDecl :: EvalContext -> SimpDecl -> State IDState ( Maybe GraphAndRef)
evalDecl c (SimpDecl l d) = case d of
  SdPatBind pat e -> Just <$> evalPatBind l c pat e
  _ -> pure Nothing

evalTopDecl :: EvalContext -> SimpDecl -> State IDState SyntaxGraph
evalTopDecl c (SimpDecl l d) = case d of
  SdPatBind pat e -> showTopLevelBind l c pat e
  SdTypeSig names typeForNames -> fst <$> evalTypeSig names typeForNames l
  SdCatchAll decl -> fst <$> makeBox (PExts.prettyPrint decl, l)

-- END END END END END evalDecl

-- TODO improve this
-- showTopLevelBind ::Show l => SimpPat l -> State IDState SyntaxGraph
showTopLevelBind ::
  SrcRef
  -> EvalContext
  -> SimpPat
  -> SimpExp
  -> State IDState SyntaxGraph
showTopLevelBind  l c pat e = do
  GraphAndRef gr originalRef <- evalPatBind l c pat e
  let ref = lookupReference (sgBinds gr) originalRef
  case ref of 
    Left _str -> do
      pure gr
    Right np -> do
      uniquePatName <- getUniqueName
      let
        patName = simpPatNameStr $ patCore pat
        icons = Set.singleton (Named uniquePatName $ mkEmbedder (SyntaxNode (BindNameNode patName) l))
        edges = Set.singleton (makeSimpleEdge (np, nameAndPort uniquePatName InputPort))
        bindGraph = syntaxGraphFromNodesEdges icons edges
      pure (bindGraph <> gr)


translateDeclToSyntaxGraph :: SimpDecl -> SyntaxGraph
translateDeclToSyntaxGraph d = deleteBindings graph where
  evaluatedDecl = evalTopDecl mempty d -- >>= showTopLevelBinds
  graph = evalState evaluatedDecl initialIdState

-- | Convert a single function declaration into a SyntaxGraph
-- Used in Unit Tests
translateStringToSyntaxGraph :: String -> SyntaxGraph
translateStringToSyntaxGraph = translateDeclToSyntaxGraph . stringToSimpDecl

-- END Exported functions
