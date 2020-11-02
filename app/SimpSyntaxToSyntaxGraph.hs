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
import qualified Data.StringMap as SMap
import qualified Data.IntMap as IMap
import           Data.Either                    ( partitionEithers )
import           Data.Maybe                     ( catMaybes
                                                , listToMaybe
                                                , fromMaybe
                                                , mapMaybe
                                                , maybeToList
                                                , isNothing
                                                , isJust
                                                )
import qualified Language.Haskell.Exts as Exts
import qualified Language.Haskell.Exts.Pretty as PExts

import           PortConstants (
  inputPort
  , resultPort
  , argumentPorts
  , caseValuePorts
  , caseConditionPorts
  , argPortsConst
  , multiIfValuePorts
  , multiIfBoolPorts
  , resultPortsConst
  , pattern ResultPortConst
  , pattern InputPortConst
  , pattern PatternValuePortConst
  , listFromPort
  , listThenPort
  , listToPort
  , listCompQualPorts
  )

import HsSyntaxToSimpSyntax(
  SimpAlt(..)
  , stringToSimpDecl
  , SrcRef(..)
  , SimpExp(..)
  , SimpExpCore(..)
  , SimpPat(..)
  , SimpPatCore(..)
  , SimpDecl(..)
  , SimpDeclCore(..)
  , SimpQStmt(..)
  , SimpQStmtCore(..)
  , qNameToString
  , nameToString
  , customParseDecl
  , SelectorAndVal(..)
  , pattern FunctionCompositionStr
  , simpPatNameStr
  )
import           Types(
  NameAndPort(..)
  , IDState
  , SgNamedNode
  , SyntaxNode(..)
  , SyntaxNodeCore(..)
  , NodeName(..)
  , LikeApplyFlavor(..)
  , CaseOrMultiIfTag(..)
  , Named(..)
  , mkEmbedder
  , Port(..)
  ,  Labeled(..)
  , Edge(..)
  , EdgeOption(..)
  , Connection(..)
  , FuncDefRegionInfo
  )
import Util(
  makeSimpleEdge
  , makeNotConstraintEdge
  , makeInvisibleEdge
  , nameAndPort
  , fmapMaybeM
  )
import SyntaxGraph( 
    SyntaxGraph(..)
  , patternName
  , syntaxGraphFromNodes
  , syntaxGraphFromNodesEdges
  , bindsToSyntaxGraph
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
  , Reference(..)
  , SgSink(..)
  , SgBind
  , grNamePortToGrRef
  , initialIdState
  , EvalContext
  , edgeFromPortToRef
  , edgeFromRefToPort
  , syntaxGraphFromEdges
  , makeAsBindGraph
  , graphsToComponents
  , combineFromPortToGraph
  , deleteBindings
  , graphAndRefToRef
  , deleteBindingsWithRef
  , combineFromGraphToPort
  )
import StringSymbols(
  listCompositionPlaceholderStr
  , typeSignatureSeparatorStr
  , typeNameSeparatorStr
  , negativeLiteralStr
  , patternWildCardStr
  , unusedArgumentStr
  , defaultPatternNameStr
  , fractionalSeparatorStr
  , showLiteral
  , showSignlessLit
  , getFuncDefLabel
  )
import FuncDefRegionInfo(getFuncDefRegionInfo)
  
{-# ANN module "HLint: ignore Use record patterns" #-}

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
  let graph = syntaxGraphFromNodes (Set.singleton (Named name (mkEmbedder (SyntaxNode (LiteralNode str) s))))
  pure (graph, nameAndPort name ResultPortConst)

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
  SeListGen from mThen mTo -> grNamePortToGrRef <$> evalListGen c l from mThen mTo

-- BEGIN apply and compose helper functions

evalFunExpAndArgs ::
  EvalContext
  -> LikeApplyFlavor
  -> (SimpExp, [SimpExp])
  -> State IDState (SyntaxGraph, NameAndPort)
evalFunExpAndArgs c flavor (funExp@(SimpExp srcRef _), argExps) = do
  funVal <- evalExp c funExp
  argVals <- mapM (evalExp c) argExps
  applyIconName <- getUniqueName
  pure
    $ makeApplyGraph srcRef (length argExps) flavor False applyIconName funVal argVals

-- END apply and compose helper functions
evalFunctionComposition ::
  EvalContext -> SrcRef-> [SimpExp] -> State IDState (SyntaxGraph, NameAndPort)
evalFunctionComposition c srcRef functions = do
  let reversedFunctios = reverse functions
  evaluatedFunctions <- mapM (evalExp c) reversedFunctios
  neverUsedPort <- Left <$> getUniqueString unusedArgumentStr
  applyIconName <- getUniqueName
  pure $ makeApplyGraph
    srcRef
    (length evaluatedFunctions)
    ComposeNodeFlavor
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

-- | Given and App expression, return
-- (argument, list composed functions)
appExpToArgFuncs :: SimpExp -> (SimpExp, [SimpExp])
appExpToArgFuncs e = case e of
  SimpExp _ (SeApp  exp1 exp2)-> (argExp, funcs <> [exp1])
    where
      (argExp, funcs) = appExpToArgFuncs exp2
  simpleExp -> (simpleExp, [])

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
  _ -> evalFunExpAndArgs c ApplyNodeFlavor (appExpToFuncArgs expr)
    
-- END evaluateAppExpression

-- BEGIN evalGeneralLet

getBoundVarName :: SimpDecl -> EvalContext
getBoundVarName (SimpDecl s d) = case d of
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
  declGraphAndRefs <- mapM ( evalDecl augmentedContext) decls
  let declGraphs = fmap graphAndRefToGraph declGraphAndRefs
  let declGraph = mconcat declGraphs
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
evalMultiIf c selectorsAndVals srcRef = do
  selAndValExps <- mapM (evalSelectorAndVal c) selectorsAndVals
  let (_srcRefs, selGraphAndRefs, valGraphAndRefs) = unzip3 selAndValExps
  graphAndNamedPort <- makeMultiIfGraph (length selectorsAndVals) srcRef selGraphAndRefs valGraphAndRefs
    <$>
    getUniqueName
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
    multiIfNode = SyntaxNode (CaseOrMultiIfNode MultiIfTag numPairs) srcRefs
    expsWithPorts = zip exps $ map (nameAndPort multiIfName) multiIfValuePorts
    boolsWithPorts = zip bools $ map (nameAndPort multiIfName) multiIfBoolPorts
    combindedGraph = combineExpressions False $ expsWithPorts <> boolsWithPorts
    icons = [Named multiIfName (mkEmbedder multiIfNode)]
    newGraph = (syntaxGraphFromNodes $ Set.fromList icons) <> combindedGraph

-- BEGIN BEGIN BEGIN BEGIN BEGIN evalCase

-- TODO patRhsAreConnected is sometimes incorrectly true if the pat is just a
-- name
-- returns (combined graph, pattern reference, rhs reference)
evalAlt ::
  EvalContext
  -> SimpAlt
  -> State IDState (Bool, SyntaxGraph, Reference, Reference, Maybe String)
evalAlt c (SimpAlt s pat rhs) = do
  ((GraphAndRef patGraph patRef, mPatAsName), GraphAndRef rhsGraph rhsRef) <-
    makePatternGraph c pat rhs
  let
    grWithEdges = makeEdgesKeepBindings makeSimpleEdge (rhsGraph <> patGraph)
    lookedUpRhsRef = lookupReference (sgBinds grWithEdges) rhsRef
    -- The pattern and rhs are conneted if makeEdges added extra edges, or if
    -- the rhsRef refers to a source in the pattern.
    patRhsAreConnected
      = (rhsRef /= lookedUpRhsRef)
        -- || ( length (sgEdges grWithEdges) > (length (sgEdges rhsGraph) + length (sgEdges patGraph)))
  pure (patRhsAreConnected
       , grWithEdges
       , patRef
       , lookedUpRhsRef
       , mPatAsName)

evalCase ::
  EvalContext -> SimpExp -> [SimpAlt]
  -> State IDState (SyntaxGraph, NameAndPort)
evalCase c simpExp@(SimpExp srcRef _) alts =
  let
    numAlts = length alts
    altSrcRefs = map simpAltRef alts
  in
    evalCaseHelper (length alts) c srcRef altSrcRefs
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
evalCaseHelper numAlts context srcRef altsSrcRefs caseIconName resultIconNames (GraphAndRef expGraph expRef) evaledAlts
  = result
  where
    (patRhsConnected, altGraphs, patRefs, rhsRefs, asNames) = unzip5 evaledAlts
    combindedAltGraph = mconcat altGraphs
    caseNode = SyntaxNode (CaseOrMultiIfNode CaseTag numAlts) srcRef
    caseNodeGraph = makeCaseNodeGraph caseIconName caseNode expRef

    bindGraph = makeAsBindGraph expRef asNames
    conditionEdgesGraph = makeConditionEdges patRefs caseIconName 
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
makeConditionEdges patRefs caseIconName = conditionEdgesGraph where
    caseConditionNamedPorts = map (nameAndPort caseIconName) caseConditionPorts
    patEdgesGraphs = zipWith (edgeFromPortToRef makeSimpleEdge) patRefs caseConditionNamedPorts
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
makeCaseResult resultIconName (rhsRef, caseValueNamedPort, srcRef) = case rhsRef of
  Left _ -> mempty
  Right rhsPort -> syntaxGraphFromNodesEdges rhsNewIcons rhsNewEdges
    where
      rhsNewIcons = Set.singleton (Named resultIconName (mkEmbedder $ SyntaxNode CaseResultNode srcRef))
      rhsNewEdges = Set.fromList [
        makeSimpleEdge (rhsPort, nameAndPort resultIconName ResultPortConst)
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
evalLambda srcRef context argPatterns expr functionName = do
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
      = getOutputNameAndPort rhsRef  argPatternVals lambdaPorts  nodesGraph -- combinedGraph 
  
    argNode = makeLambdaArgumentNode argPatternValsWithAsNames srcRef
    lambdaNode = makeLambdaNode nodesGraph {- was combinedGraph -} lambdaLabel [lambdaName, argNodeName] srcRef
    lambdaPorts = map (nameAndPort argNodeName) $ argumentPorts lambdaNode
    lambdaValueLink =  makeEitherEdgeOrSgBind outputReference (nameAndPort lambdaName (inputPort lambdaNode))
  -- (valueGraph,outputNameAndPort) <- getValueGraphAndNamedPort outputReference

    -- lambdaValueEdge = makeSimpleEdge (outputNameAndPort,)
    -- TODO move adding drawing rank edge after graph simplification and collapsing
    constraintEdgeList = constraintLambdaArgAboveValue outputReference argNodeName lambdaName

    (argPatternEdges', newBinds') =
      partitionEithers $ zipWith makePatternEdgeInLambda argPatternVals lambdaPorts
    lambdaEdges = (constraintEdgeList ++ argPatternEdges')

    lambdaArgGraph = makeLambdaArgGraph lambdaEdges newBinds' (argNodeName,argNode)
    lambdaIconAndOutputGraph = makeLambdaOutputGraph (lambdaName ,lambdaNode) [lambdaValueLink] lambdaLabel

    asBindGraph = mconcat $ zipWith asBindGraphZipper (fmap snd argPatternValsWithAsNames) lambdaPorts

    combinedGraph = makeEdges makeSimpleEdge (rhsRawGraph <> argPatternGraph <> lambdaArgGraph <> asBindGraph )
    finalGraph = makeEdges makeNotConstraintEdge (combinedGraph <> lambdaIconAndOutputGraph)

    resultNameAndPort = nameAndPort lambdaName (resultPort lambdaNode)
  if isIdLambda isOutputStraightFromInput argPatterns functionName
  then makeBox ( Set.elemAt 0 argPatternStrings, srcRef)
  else pure (finalGraph, resultNameAndPort)

constraintLambdaArgAboveValue :: Reference -> NodeName -> NodeName -> [Edge]
constraintLambdaArgAboveValue outputReference argNodeName lambdaName = -- case outputReference of 
    [makeInvisibleEdge (nameAndPort argNodeName ResultPortConst,
      nameAndPort lambdaName InputPortConst)]
    

isIdLambda ::  Bool -> [SimpPat] -> Maybe String -> Bool
isIdLambda isOutputStraightFromInput argPatterns functionName
  = isOutputStraightFromInput && length argPatterns == 1 && isNothing functionName 

getOutputNameAndPort :: Reference
  -> [GraphAndRef]
  -> [NameAndPort]
  -> SyntaxGraph
  -> (Reference, Bool)
getOutputNameAndPort (Right np)        _              _           _            = (Right np, False)
getOutputNameAndPort strRef@(Left str) argPatternVals lambdaPorts allNodeGraps = (maybeNamedPort, isItoO) where
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

makeLambdaNode :: SyntaxGraph -> String -> [NodeName] -> SrcRef -> SyntaxNode
makeLambdaNode combinedGraph  functionName lambdaNames srcRef = node where
  regionInfo = getFuncDefRegionInfo combinedGraph lambdaNames
  node = SyntaxNode (FunctionValueNode functionName regionInfo) srcRef

makeLambdaArgumentNode :: [(GraphAndRef, Maybe String)] -> SrcRef -> SyntaxNode
makeLambdaArgumentNode argPatternValsWithAsNames srcRef = node where 
  paramNames = fmap patternName argPatternValsWithAsNames
  node = SyntaxNode (FunctionArgNode paramNames) srcRef


makeLambdaOutputGraph :: (NodeName, SyntaxNode)
  -> [Either Edge (SMap.Key, Reference)]
  -> String
  -> SyntaxGraph
makeLambdaOutputGraph  (lambdaName ,lambdaNode) lambdaValueLink functionName = graph where
  (valueEdges', valueBinds') = partitionEithers lambdaValueLink
  valueEdges = Set.fromList valueEdges'
  bindForRecursion = (functionName, (Right $ nameAndPort lambdaName ResultPortConst))
  valueBinds = SMap.fromList (bindForRecursion : valueBinds')
  lambdaIconSet = Set.singleton (Named lambdaName (mkEmbedder lambdaNode))
  graph = SyntaxGraph lambdaIconSet valueEdges mempty valueBinds mempty


makeLambdaArgGraph :: [Edge]
                        -> [(SMap.Key, Reference)] -> (NodeName, SyntaxNode) -> SyntaxGraph
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
  Right (NameAndPort name _) 
    -> Left $ makeSimpleEdge (lamPort, patternValueInputPort name )
  Left str -> Right (str, Right lamPort)

patternValueInputPort :: NodeName -> NameAndPort
patternValueInputPort name = NameAndPort name  PatternValuePortConst
-- END END END END END evalLambda 

makeApplyGraph ::
  SrcRef
  -> Int
  -> LikeApplyFlavor
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
      = map (nameAndPort applyIconName) (argumentPorts applyNode)
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

-- TODO add PatternValuePortConst to all
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
makeNestedPatternGraph applyIconName funStr argVals srcRef = nestedApplyResult
  where
    dummyNode = SyntaxNode (PatternApplyNode defaultPatternNameStr []) srcRef

    argsAndPorts
      = zip argVals $ map (nameAndPort applyIconName) $ argumentPorts dummyNode
    mappedArgs = fmap patternArgumentMapper argsAndPorts

    (unnestedArgsAndPort, nestedNamedNodesAndGraphs)
      = partitionEithers (fmap snd mappedArgs)

    (nestedArgs, _, nestedSinks, nestedBinds, nestedEMaps)
      = graphsToComponents $ fmap snd nestedNamedNodesAndGraphs

    argListMapper (str, arg) = case arg of
      Left _ -> Labeled Nothing str
      Right (namedNode, _) -> Labeled (Just namedNode) str

    argList = fmap argListMapper mappedArgs

    combinedGraph = combineExpressions True unnestedArgsAndPort

    pAppNode = SyntaxNode (PatternApplyNode funStr argList) srcRef
    icons = Set.singleton (Named applyIconName (mkEmbedder pAppNode))

    asNameBinds = mapMaybe asNameBind argVals
    allBinds = SMap.union nestedBinds (SMap.fromList asNameBinds)

    newEMap = IMap.fromList
              ((\(Named (NodeName n) _) -> (n, applyIconName))  <$> (Set.toList nestedArgs))
              <> nestedEMaps

    newGraph = SyntaxGraph
               icons
               Set.empty
               nestedSinks
               allBinds
               newEMap
    nestedApplyResult = (newGraph <> combinedGraph
                        , nameAndPort applyIconName (resultPort pAppNode))

patternArgumentMapper ::
  ((GraphAndRef, Maybe String), t)
  -> (String, Either (GraphAndRef, t) (SgNamedNode, SyntaxGraph))
patternArgumentMapper (asGraphAndRef@(graphAndRef, _), port)
  = (patName, eitherVal)
  where
    graph = graphAndRefToGraph graphAndRef
    patName = patternName asGraphAndRef

    nodes = sgNodes graph
    eitherVal = if Set.size nodes == 1 && Set.size (sgEdges graph) == 0
      then Right ((Set.elemAt 0 nodes), graph)
      else Left (graphAndRef, port)

asNameBind :: (GraphAndRef, Maybe String) -> Maybe SgBind
asNameBind (GraphAndRef _ ref, mAsName) = case mAsName of
  Nothing -> Nothing
  Just asName -> Just ( asName, ref)

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

-- END END END END END evalPattern
evalListGen :: EvalContext -> SrcRef -> SimpExp -> Maybe SimpExp -> Maybe SimpExp -> State IDState (SyntaxGraph, NameAndPort)
evalListGen c l from mThen mTo = do
  listGenName <- getUniqueName
  fromGraphAndRef <- evalExp c from
  maybeThenGraphAndRef <- fmapMaybeM (evalExp c) mThen
  maybeToGraphAndRef <- fmapMaybeM (evalExp c) mTo
  let 
    listGenNode = SyntaxNode (ListGenNode (isJust mThen) (isJust mTo)) l 
    edgesGraph = makeListGenEdges listGenName fromGraphAndRef maybeThenGraphAndRef maybeToGraphAndRef
    resultNameAndPort = nameAndPort listGenName (resultPort listGenNode)
    listGenNodeGraph = syntaxGraphFromNodes (Set.singleton (Named listGenName (mkEmbedder listGenNode)))
    thenToGraphs = mconcat $ catMaybes [(fmap graph maybeThenGraphAndRef), (fmap graph maybeToGraphAndRef)]
    finalGraph = makeEdges makeSimpleEdge $ listGenNodeGraph <> edgesGraph <> graph fromGraphAndRef <> thenToGraphs
  pure (finalGraph, resultNameAndPort)

makeListGenEdges :: NodeName -> GraphAndRef -> Maybe GraphAndRef -> Maybe GraphAndRef -> SyntaxGraph
makeListGenEdges listGenName (GraphAndRef _ refFrom) maybeThenGraphAndRef maybeToGraphAndRef = mconcat $ edgeFrom : edgeThen ++ edgeTo where
  edgeFrom = edgeFromRefToPort makeSimpleEdge refFrom (nameAndPort listGenName listFromPort)
  edgeThen = maybeToList $ fmap ((flip (edgeFromRefToPort makeSimpleEdge)) (nameAndPort listGenName listThenPort)) (fmap ref maybeThenGraphAndRef)
  edgeTo = maybeToList $ fmap ((flip (edgeFromRefToPort makeSimpleEdge)) (nameAndPort listGenName listToPort)) (fmap ref maybeToGraphAndRef) 

-- BEGIN BEGIN BEGIN BEGIN BEGIN list comp
-- valus form ListCompNode are connected to item constructor
-- TODO reconsider PORT architecture choise to identfy arguments
-- TODO improve connection to guard expresion
-- TODO connect valus from generators to ListCompNode 
evalListComp ::
  EvalContext -> SrcRef -> SimpExp -> [SimpQStmt] -> State IDState GraphAndRef
evalListComp context l  itemExp qualExps =  do  
  
  let decls = [d | (SimpQStmt srcRef (SqLet d )) <- qualExps]
  declGraphAndRefdeclContexts <-  mapM (evalDecls context) decls -- TODO add decls to context and graph
  let (declGraphAndRef, declContexts) = unzip declGraphAndRefdeclContexts
  let declContext =  Set.unions (context : declContexts)
  

  let gens  = [(srcRef, x) | (SimpQStmt srcRef x@(SqGen {})) <- qualExps]
  genGRContextsAndGRpatRef <- mapM (evalSqGen declContext)  gens
  let genGraphsAndRefs =  fmap snd genGRContextsAndGRpatRef
  let genContext = Set.unions (declContext : (map (namesInPattern . fst) genGRContextsAndGRpatRef))

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
evalSqGen context (l, (SqGen values itemPat)) = do 
  (GraphAndRef patternGraph patternRef, itemContext) <- evalPattern itemPat
  (GraphAndRef valueGraph valueRef) <- evalExp context values -- TODO use "pat" to put value into item constructor
  let graph = valueGraph <> patternGraph
  pure ((GraphAndRef graph patternRef, itemContext),(GraphInPatternRef graph valueRef patternRef))

evalSqGen _ _ = error "SimpQStmt must be SqGen" 

makeListCompGraph :: EvalContext -> NodeName -> SrcRef -> GraphAndRef
  -> [GraphAndRef] -> [GraphAndRef] -> [GraphInPatternRef] -> GraphAndRef
makeListCompGraph context listCompName listCompSrcRef listCompItemGraphAndRef
  qualGraphsAndRefs declGraphsAndRefs genGraphsAndRefs 
  = GraphAndRef combinedGraph (Right listCompNodeRef) where

  (listCompNode, listCompNodeRef, listCompNodeGraph) 
    = makeListCompNodeGraph listCompName listCompSrcRef (length genGraphsAndRefs) (length qualGraphsAndRefs)

  listCompItemGraph = combineFromGraphToPort makeSimpleEdge
    listCompItemGraphAndRef ( NameAndPort listCompName  (inputPort listCompNode))

  qStmtGraph = makeQstmtGraph qualGraphsAndRefs declGraphsAndRefs genGraphsAndRefs 

  qualEdgeGraph = makeListCompQualEdgeGraph listCompName qualGraphsAndRefs

  listCompGenEdgeGraph = makeListCompGenEdgeGraph listCompName genGraphsAndRefs

  combinedGraph = makeEdges makeSimpleEdge
    (listCompItemGraph  <> listCompNodeGraph <> qStmtGraph <> qualEdgeGraph <> listCompGenEdgeGraph) 

makeListCompQualEdgeGraph listCompName qualGraphsAndRefs = qualEdgeGraph where 
  listCompQualNamedPorts = map (nameAndPort listCompName) listCompQualPorts
  qualGraphRefAndPortTo = zip qualGraphsAndRefs listCompQualNamedPorts
  qualEdgeGraph = mconcat $ map (uncurry (combineFromGraphToPort makeSimpleEdge)) qualGraphRefAndPortTo

makeListCompGenEdgeGraph listCompName genGraphsAndRefs = inEdgeGraph <> outEdgeGraph where
  listCompInNamedPorts = map (nameAndPort listCompName) argPortsConst
  listCompOutNamedPorts = map (nameAndPort listCompName) resultPortsConst

  graphAndValueRef = map graphInPatternRefToGraphAndRef genGraphsAndRefs
  graphAndPatternRef = map graphInPatternRefToGraphAndPat genGraphsAndRefs

  inEdgeGraph = mconcat $ map (uncurry (combineFromGraphToPort makeSimpleEdge)) (zip graphAndValueRef listCompInNamedPorts)
  outEdgeGraph = mconcat $ map (uncurry (combineFromPortToGraph makeSimpleEdge)) (zip graphAndPatternRef listCompOutNamedPorts)


makeListCompNodeGraph listCompName listCompSrcRef genCount qualCount = (listCompNode, listCompNodeRef, listCompNodeGraph) where
  listCompNode = SyntaxNode (ListCompNode genCount qualCount) listCompSrcRef
  listCompNodeRef = nameAndPort listCompName (resultPort listCompNode)
  listCompNodeGraph = syntaxGraphFromNodes
    $ Set.singleton (Named listCompName (mkEmbedder listCompNode))

makeQstmtGraph qualGraphsAndRefs declGraphsAndRefs genGraphsAndRefs = genGraphs <> qualGraphs where
  qualGraphs = mconcat $ fmap  graphAndRefToGraph (qualGraphsAndRefs ++ declGraphsAndRefs)
  genGraphs = mconcat $ fmap  graphInPatternRefToGraph genGraphsAndRefs

data GraphInPatternRef = GraphInPatternRef{
  syntaxGraph :: SyntaxGraph
  , valueReference ::  Reference
  , inPatternRef ::  Reference
  }

graphInPatternRefToGraph :: GraphInPatternRef -> SyntaxGraph
graphInPatternRefToGraph (GraphInPatternRef g _ _) = g

graphInPatternRefToGraphAndRef :: GraphInPatternRef -> GraphAndRef
graphInPatternRefToGraphAndRef (GraphInPatternRef g r _) = (GraphAndRef g r)

graphInPatternRefToGraphAndPat :: GraphInPatternRef -> GraphAndRef
graphInPatternRefToGraphAndPat (GraphInPatternRef g _ p) = (GraphAndRef g p)
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
evalPatBindHelper :: Reference -> Reference -> (Set.Set Edge, Set.Set SgSink, SMap.StringMap Reference)
evalPatBindHelper patRef rhsRef = case patRef of
  (Left s) -> (mempty, mempty, SMap.singleton s rhsRef)
  (Right (NameAndPort name _)) -> 
    let patternValuePort = patternValueInputPort name in
      case rhsRef of
      (Left rhsStr) -> (mempty, Set.singleton (SgSink rhsStr patternValuePort), SMap.empty)
      (Right rhsPort) -> (Set.singleton (makeSimpleEdge (rhsPort, patternValuePort)), mempty, SMap.empty)

-- Pretty printing the entire type sig results in extra whitespace in the middle
evalTypeSig :: [Exts.Name Exts.SrcSpanInfo] -> Exts.Type Exts.SrcSpanInfo -> SrcRef
  -> State IDState (SyntaxGraph, NameAndPort)
evalTypeSig names typeForNames srcRef= makeBox
  (intercalate typeNameSeparatorStr (fmap prettyPrintWithoutNewlines names)
   ++ typeSignatureSeparatorStr
   ++ prettyPrintWithoutNewlines typeForNames
   , srcRef)
  where
    -- TODO Make custom version of prettyPrint for type signitures.
    -- Use (unwords . words) to convert consecutive whitspace characters to one
    -- space.
    prettyPrintWithoutNewlines = unwords . words . Exts.prettyPrint

evalDecl :: EvalContext -> SimpDecl -> State IDState GraphAndRef
evalDecl c (SimpDecl l d) = case d of
  SdPatBind pat e -> evalPatBind l c pat e
  _ -> error "TODO reference for inner graphs"
  -- SdTypeSig _ names typeForNames -> fst <$> evalTypeSig names typeForNames
  -- SdCatchAll decl -> fst <$> makeBox (PExts.prettyPrint decl)

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
showTopLevelBind  l c pat@(SimpPat _ patCore) e = do
  GraphAndRef gr originalRef <- evalPatBind l c pat e
  let ref = lookupReference (sgBinds gr) originalRef
  case ref of 
    Left str -> do
      pure (gr)
    Right np -> do
      uniquePatName <- getUniqueName
      let
        patName = simpPatNameStr patCore
        icons = Set.singleton (Named uniquePatName $ mkEmbedder (SyntaxNode (BindNameNode patName) l))
        edges = Set.singleton (makeSimpleEdge (np, nameAndPort uniquePatName InputPortConst))
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
