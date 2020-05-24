{-# LANGUAGE NoMonomorphismRestriction, TupleSections, PatternSynonyms #-}

module SimpSyntaxToSyntaxGraph(
  translateStringToSyntaxGraph,
  translateDeclToSyntaxGraph,
  customParseDecl
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
  , pattern PatternValuePortConst
  )

import HsSyntaxToSimpSyntax(
  SimpAlt(..)
  , stringToSimpDecl
  , SimpExp(..)
  , SimpPat(..)
  , qNameToString
  , nameToString
  , customParseDecl
  , SimpDecl(..)
  , SelectorAndVal(..)
  , pattern FunctionCompositionStr
  , SimpQStmt(..)
  , simpPatNameStr
  )
import           Types(
  NameAndPort(..)
  , IDState
  , SgNamedNode
  , SyntaxNode(..)
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
  )
import Util(
  makeSimpleEdge
  , makeNotConstraintEdge
  , makeInvisibleEdge
  , nameAndPort
  , justName
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
  , edgeForRefPortIsSource
  , edgeForRefPortIsNotSource
  , syntaxGraphFromEdges
  , makeAsBindGraph
  , graphsToComponents
  , combineExpresionsIsSource
  , deleteBindings
  , graphAndRefToRef
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

makeBox :: String -> State IDState (SyntaxGraph, NameAndPort)
makeBox str = do
  name <- getUniqueName
  let graph
        = syntaxGraphFromNodes (Set.singleton (Named name (mkEmbedder (LiteralNode str))))
  pure (graph, justName name)

evalLit = makeBox . showSignlessLit

evalExp :: Show l => EvalContext -> SimpExp l -> State IDState GraphAndRef
evalExp c x = case x of
  SeName _ s -> strToGraphRef c s
  SeLit _ lit -> grNamePortToGrRef <$> evalLit lit
  SeApp _ _ _ -> grNamePortToGrRef <$> evalApp c x
  SeLambda l argPatterns e functionName -> grNamePortToGrRef <$> evalLambda l c argPatterns e functionName
  SeLet _ decls expr -> evalLet c decls expr
  SeCase _ expr alts -> grNamePortToGrRef <$> evalCase c expr alts
  SeMultiIf _ selectorsAndVals
    -> grNamePortToGrRef <$> evalMultiIf c selectorsAndVals
  SeListComp l e eList -> evalListComp c l e eList

-- BEGIN apply and compose helper functions

evalFunExpAndArgs :: Show l =>
  EvalContext
  -> LikeApplyFlavor
  -> (SimpExp l, [SimpExp l])
  -> State IDState (SyntaxGraph, NameAndPort)
evalFunExpAndArgs c flavor (funExp, argExps) = do
  funVal <- evalExp c funExp
  argVals <- mapM (evalExp c) argExps
  applyIconName <- getUniqueName
  pure
    $ makeApplyGraph (length argExps) flavor False applyIconName funVal argVals

-- END apply and compose helper functions
evalFunctionComposition :: Show l =>
  EvalContext -> [SimpExp l] -> State IDState (SyntaxGraph, NameAndPort)
evalFunctionComposition c functions = do
  let reversedFunctios = reverse functions
  evaluatedFunctions <- mapM (evalExp c) reversedFunctios
  neverUsedPort <- Left <$> getUniqueString unusedArgumentStr
  applyIconName <- getUniqueName
  pure $ makeApplyGraph
    (length evaluatedFunctions)
    ComposeNodeFlavor
    False
    applyIconName
    (GraphAndRef mempty neverUsedPort)
    evaluatedFunctions

-- | Turn (a . b . c) into [a, b, c]
compositionToList :: SimpExp l -> [SimpExp l]
compositionToList e = case e of
  (SeApp _ (SeApp _ (SeName  _ FunctionCompositionStr) f1) f2)
    -> f1 : compositionToList f2
  x -> [x]

-- BEGIN evaluateAppExpression

-- | Given two expressions f and x, where f is applied to x,
-- return the nesting depth if (f x) is rendered with
-- the (normal apply icon, compose apply icon)
applyComposeScoreHelper :: SimpExp l -> SimpExp l -> (Int, Int)
applyComposeScoreHelper exp1 exp2 = (appScore, compScore) where
  (e1App, e1Comp) = applyComposeScore exp1
  (e2App, e2Comp) = applyComposeScore exp2

  leftApp = min e1App (1 + e1Comp)
  rightApp = 1 + min e2App e2Comp

  appScore = max leftApp rightApp

  leftComp = 1 + min e1App e1Comp
  rightComp = min (1 + e2App) e2Comp

  compScore = max leftComp rightComp


-- TODO Consider putting this logic in a separate "simplifyExpression" function.
-- | Returns the amount of nesting if the App is converted to
-- (applyNode, composeNode)
applyComposeScore :: SimpExp l -> (Int, Int)
applyComposeScore e = case e of
  SeApp _ exp1 exp2 -> applyComposeScoreHelper exp1 exp2
  _ -> (0, 0)

-- TODO add test for this function
-- | Given an App expression, return
-- (function, list of arguments)
appExpToFuncArgs :: SimpExp l -> (SimpExp l, [SimpExp l])
appExpToFuncArgs e = case e of
  SeApp _ exp1 exp2 -> (funExp, args <> [exp2])
    where
      (funExp, args) = appExpToFuncArgs exp1
  x -> (x, [])

-- | Given and App expression, return
-- (argument, list composed functions)
appExpToArgFuncs :: SimpExp l -> (SimpExp l, [SimpExp l])
appExpToArgFuncs e = case e of
  SeApp _ exp1 exp2 -> (argExp, funcs <> [exp1])
    where
      (argExp, funcs) = appExpToArgFuncs exp2
  simpleExp -> (simpleExp, [])

-- TODO Refactor this and all sub-expressions
evalApp :: Show l =>
  EvalContext -> SimpExp l
  -> State IDState (SyntaxGraph, NameAndPort)
evalApp c expr = case expr of
  -- TODO This pattern for "." appears at least twice in this file. Refactor?
  (SeApp _ (SeApp _ (SeName  _ FunctionCompositionStr) _) _)
    -> evalFunctionComposition c (compositionToList expr)
  _ -> if appScore <= compScore
    then evalFunExpAndArgs c ApplyNodeFlavor (appExpToFuncArgs expr)
    else evalFunExpAndArgs c ComposeNodeFlavor (appExpToArgFuncs expr)
    where
      (appScore, compScore) = applyComposeScore expr

-- END evaluateAppExpression

-- BEGIN evalGeneralLet

getBoundVarName :: Show l => SimpDecl l -> EvalContext
getBoundVarName d = case d of
  SdPatBind _ pat _ -> namesInPattern
                     -- TODO Should evalState be used here?
                     $ evalState (evalPattern pat) initialIdState
  SdTypeSig _ _ _ -> Set.empty
  SdCatchAll _ -> Set.empty

-- TODO make it rerurn -> State IDState [(SyntaxGraph, EvalContext)]
evalDecls :: Show l =>
  EvalContext -> [SimpDecl l] -> State IDState (SyntaxGraph, EvalContext)
evalDecls c decls = do
  let
    boundNames = Set.unions (fmap getBoundVarName decls)
    augmentedContext = Set.union boundNames c
  declGraphAndRefs <- mapM ( evalDecl augmentedContext) decls
  let declGraphs = fmap graphAndRefToGraph declGraphAndRefs
  let declGraph = mconcat declGraphs
  pure (declGraph, augmentedContext)

evalLet :: Show l =>
  EvalContext
  -> [SimpDecl l]
  -> SimpExp l
  -> State IDState GraphAndRef
evalLet c decls expr = do
  (bindGraph, bindContext) <- evalDecls c decls
  expVal <- evalExp bindContext expr
  let
    GraphAndRef expGraph expResult = expVal
    newGraph = makeEdgesKeepBindings c $ expGraph <> bindGraph
    bindings = sgBinds bindGraph
  pure $ GraphAndRef newGraph (lookupReference bindings expResult)

-- END evalGeneralLet

evalSelectorAndVal :: Show l =>
  EvalContext -> SelectorAndVal l -> State IDState (GraphAndRef, GraphAndRef)
evalSelectorAndVal c SelectorAndVal{svSelector=sel, svVal=val}
  = (,) <$> evalExp c sel <*> evalExp c val

evalMultiIf :: Show l =>
  EvalContext -> [SelectorAndVal l] -> State IDState (SyntaxGraph, NameAndPort)
evalMultiIf c selectorsAndVals = let
  evaledRhss = unzip <$> mapM (evalSelectorAndVal c) selectorsAndVals
  in
  makeMultiIfGraph (length selectorsAndVals)
  <$>
  getUniqueName
  <*>
  fmap fst evaledRhss
  <*>
  fmap snd evaledRhss

makeMultiIfGraph ::
  Int
  -> NodeName
  -> [GraphAndRef]
  -> [GraphAndRef]
  -> (SyntaxGraph, NameAndPort)
makeMultiIfGraph numPairs multiIfName bools exps
  = (newGraph, nameAndPort multiIfName (resultPort multiIfNode))
  where
    multiIfNode = CaseOrMultiIfNode MultiIfTag numPairs
    expsWithPorts = zip exps $ map (nameAndPort multiIfName) multiIfValuePorts
    boolsWithPorts = zip bools $ map (nameAndPort multiIfName) multiIfBoolPorts
    combindedGraph = combineExpressions False $ expsWithPorts <> boolsWithPorts
    icons = [Named multiIfName (mkEmbedder multiIfNode)]
    newGraph = (syntaxGraphFromNodes $ Set.fromList icons) <> combindedGraph

-- BEGIN BEGIN BEGIN BEGIN BEGIN evalCase

-- TODO patRhsAreConnected is sometimes incorrectly true if the pat is just a
-- name
-- returns (combined graph, pattern reference, rhs reference)
evalAlt :: Show l =>
  EvalContext
  -> SimpAlt l
  -> State IDState (Bool, SyntaxGraph, Reference, Reference, Maybe String)
evalAlt c (SimpAlt pat rhs) = do
  ((GraphAndRef patGraph patRef, mPatAsName), GraphAndRef rhsGraph rhsRef) <-
    makePatternGraph c pat rhs
  let
    grWithEdges = makeEdgesKeepBindings c (rhsGraph <> patGraph)
    lookedUpRhsRef = lookupReference (sgBinds grWithEdges) rhsRef
    -- The pattern and rhs are conneted if makeEdges added extra edges, or if
    -- the rhsRef refers to a source in the pattern.
    patRhsAreConnected
      = (rhsRef /= lookedUpRhsRef)
        -- || ( length (sgEdges grWithEdges) > (length (sgEdges rhsGraph) + length (sgEdges patGraph)))
  pure (patRhsAreConnected
       , deleteBindings grWithEdges
       , patRef
       , lookedUpRhsRef
       , mPatAsName)

evalCase :: Show l 
  => EvalContext -> SimpExp l -> [SimpAlt l]
  -> State IDState (SyntaxGraph, NameAndPort)
evalCase c e alts =
  let
    numAlts = length alts
  in
    evalCaseHelper (length alts) c
    <$>
    getUniqueName
    <*>
    replicateM numAlts getUniqueName
    <*>
    evalExp c e
    <*>
    mapM (evalAlt c) alts

evalCaseHelper ::
  Int
  -> EvalContext
  -> NodeName
  -> [NodeName]
  -> GraphAndRef
  -> [(Bool, SyntaxGraph, Reference, Reference, Maybe String)]
  -> (SyntaxGraph, NameAndPort)
evalCaseHelper numAlts context caseIconName resultIconNames (GraphAndRef expGraph expRef) evaledAlts 
  = result
  where
    (patRhsConnected, altGraphs, patRefs, rhsRefs, asNames) = unzip5 evaledAlts
    combindedAltGraph = mconcat altGraphs
    caseNode = CaseOrMultiIfNode CaseTag numAlts
    caseNodeGraph = makeCaseNodeGraph caseIconName caseNode expRef

    bindGraph = makeAsBindGraph expRef asNames
    conditionEdgesGraph = makeConditionEdges patRefs caseIconName 
    caseResultGraphs = makeRhsGraph patRhsConnected rhsRefs caseIconName resultIconNames

    finalGraph = makeEdges context $ mconcat [
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
    inputEdgeGraph = edgeForRefPortIsNotSource makeSimpleEdge expRef caseNodeNameAndPort
    caseGraph = inputEdgeGraph <> syntaxGraphFromNodes icons

makeConditionEdges :: [Reference] -> NodeName -> SyntaxGraph
makeConditionEdges patRefs caseIconName = conditionEdgesGraph where
    caseConditionNamedPorts = map (nameAndPort caseIconName) caseConditionPorts
    patEdgesGraphs = zipWith (edgeForRefPortIsSource makeSimpleEdge) patRefs caseConditionNamedPorts
    conditionEdgesGraph = mconcat patEdgesGraphs

makeRhsGraph :: [Bool] -> [Reference ] -> NodeName -> [NodeName] ->  SyntaxGraph
makeRhsGraph patRhsConnected rhsRefs caseIconName resultIconNames = caseResultGraphs where
  caseValueNamedPorts = map (nameAndPort caseIconName) caseValuePorts
  isConnectedAndValueRefAndNamedPorts =  zip3 patRhsConnected rhsRefs caseValueNamedPorts

  (connected, unConnected) = partition (\(x,_,_) -> x) isConnectedAndValueRefAndNamedPorts
  unConnectedRhss = map (\(_,x,y) -> (x,y)) unConnected
  connectedRhss = map (\(_,x,y) -> (x,y)) connected

  caseEdgeGraph = mconcat $ fmap (uncurry (edgeForRefPortIsNotSource makeSimpleEdge)) unConnectedRhss

  resultNodeGraph = mconcat $ zipWith  makeCaseResult resultIconNames connectedRhss

  caseResultGraphs = caseEdgeGraph <> resultNodeGraph

makeCaseResult :: NodeName -> (Reference, NameAndPort) -> SyntaxGraph
makeCaseResult resultIconName (rhsRef, caseValueNamedPort) = case rhsRef of
  Left _ -> mempty
  Right rhsPort -> syntaxGraphFromNodesEdges rhsNewIcons rhsNewEdges
    where
      rhsNewIcons = Set.singleton (Named resultIconName (mkEmbedder CaseResultNode))
      rhsNewEdges = Set.fromList [
        makeSimpleEdge (rhsPort, justName resultIconName)
        , makeNotConstraintEdge ( nameAndPort resultIconName (Port 1), caseValueNamedPort)
        ]
-- END END END END END evalCase

-- BEGIN BEGIN BEGIN BEGIN BEGIN evalLambda 
evalLambda :: Show l
  => l
  -> EvalContext
  -> [SimpPat l]
  -> SimpExp l
  -> Maybe String
  -> State IDState (SyntaxGraph, NameAndPort)
evalLambda _ context argPatterns expr functionName = do
  lambdaName <- getUniqueName
  argNodeName <- getUniqueName
  argPatternValsWithAsNames <- mapM evalPattern argPatterns
  let
    lambdaLabel = getFuncDefLabel lambdaName functionName
    argPatternStrings = Set.unions $ fmap namesInPattern argPatternValsWithAsNames
    rhsContext = Set.union argPatternStrings context
  GraphAndRef rhsRawGraph rhsRef <- evalExp rhsContext expr
  let
    argPatternVals = fmap fst argPatternValsWithAsNames
    argPatternGraph = mconcat $ fmap graphAndRefToGraph argPatternVals
    nodesGraph = argPatternGraph <> rhsRawGraph
    (outputReference,isOutputStraightFromInput) 
      = getOutputNameAndPort rhsRef  argPatternVals lambdaPorts  nodesGraph -- combinedGraph 
  
    argNode = makeLambdaArgumentNode argPatternValsWithAsNames
    lambdaNode = makeLambdaNode nodesGraph {- was combinedGraph -} lambdaLabel [lambdaName, argNodeName]
    lambdaPorts = map (nameAndPort argNodeName) $ argumentPorts lambdaNode
  (valueGraph,outputNameAndPort) <- getValueGraphAndNamedPort outputReference
  let
    lambdaValueEdge = makeSimpleEdge (outputNameAndPort, nameAndPort lambdaName (inputPort lambdaNode))
    -- TODO move adding drawing rank edge after graph simplification and collapsing
    constraintEdgeList = constraintLambdaArgAboveValue outputReference argNodeName lambdaName
    lambdaEdges = (lambdaValueEdge : constraintEdgeList ++ argPatternEdges')

    (argPatternEdges', newBinds') =
      partitionEithers $ zipWith makePatternEdgeInLambda argPatternVals lambdaPorts

    lambdaIconAndOutputGraph
      = makeLambdaOutputGraph lambdaEdges newBinds' (lambdaName ,lambdaNode) (argNodeName,argNode)

    asBindGraph = mconcat $ zipWith asBindGraphZipper (fmap snd argPatternValsWithAsNames) lambdaPorts

    combinedGraph = (asBindGraph <> rhsRawGraph <> argPatternGraph <> lambdaIconAndOutputGraph <> valueGraph)
    finalGraph = makeEdges (Set.insert lambdaLabel context) combinedGraph 

    resultNameAndPort = nameAndPort lambdaName (resultPort lambdaNode)
  if isIdLambda isOutputStraightFromInput argPatterns functionName
  then makeBox $ Set.elemAt 0 argPatternStrings
  else pure (finalGraph, resultNameAndPort)

constraintLambdaArgAboveValue :: Reference -> NodeName -> NodeName -> [Edge]
constraintLambdaArgAboveValue outputReference argNodeName lambdaName= case outputReference of 
    Left _str -> [makeInvisibleEdge (justName argNodeName, justName lambdaName)]
    _ -> []

isIdLambda ::  Bool -> [SimpPat l] -> Maybe String -> Bool
isIdLambda isOutputStraightFromInput argPatterns functionName
  = isOutputStraightFromInput && length argPatterns == 1 && functionName == Nothing

-- TODO change to just making bind if not present in graph bind
getValueGraphAndNamedPort :: Reference -> State IDState (SyntaxGraph, NameAndPort)
getValueGraphAndNamedPort outputReference = do
  case outputReference of 
    Right np -> do
      getUniqueName
      pure (mempty , np)
    Left str -> makeBox str

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

makeLambdaNode :: SyntaxGraph -> String -> [NodeName] -> SyntaxNode
makeLambdaNode combinedGraph  functionName lambdaNames = node where
  allNodeNames = Set.map naName (sgNodes combinedGraph)
  enclosedNodeNames =  Set.difference allNodeNames (Set.fromList lambdaNames)
  node = FunctionValueNode functionName enclosedNodeNames

makeLambdaArgumentNode :: [(GraphAndRef, Maybe String)] -> SyntaxNode
makeLambdaArgumentNode argPatternValsWithAsNames = node where 
  paramNames = fmap patternName argPatternValsWithAsNames
  node = FunctionArgNode paramNames


makeLambdaOutputGraph ::
  [Edge] 
  -> [(SMap.Key, Reference)] 
  -> (NodeName, SyntaxNode)
  -> (NodeName, SyntaxNode)
  -> SyntaxGraph
makeLambdaOutputGraph argPatternEdgesList binds (lambdaName ,lambdaNode) (argNodeName,argNode) = graph where
  argPatternEdges = Set.fromList argPatternEdgesList
  bindsSet = SMap.fromList binds
  lambdaIconSet = Set.fromList [
    (Named lambdaName (mkEmbedder lambdaNode))
    , (Named argNodeName (mkEmbedder argNode))
    ]
  graph = SyntaxGraph lambdaIconSet argPatternEdges mempty bindsSet mempty

-- lambda
makePatternEdgeInLambda ::GraphAndRef -> NameAndPort -> Either Edge SgBind
makePatternEdgeInLambda (GraphAndRef _ ref) lamPort = case ref of
  Right (NameAndPort name _) 
    -> Left $ makeSimpleEdge (lamPort, patternValueInputPort name )
  Left str -> Right (str, Right lamPort)

patternValueInputPort :: NodeName -> NameAndPort
patternValueInputPort name = NameAndPort name (Just PatternValuePortConst)
-- END END END END END evalLambda 

makeApplyGraph ::
  Int
  -> LikeApplyFlavor
  -> Bool
  -> NodeName
  -> GraphAndRef
  -> [GraphAndRef]
  -> (SyntaxGraph, NameAndPort)
makeApplyGraph numArgs applyFlavor inPattern applyIconName funVal argVals
  = (newGraph <> combinedGraph
    , nameAndPort applyIconName (resultPort applyNode)
    )
  where
    applyNode = ApplyNode applyFlavor numArgs
    argumentNamePorts
      = map (nameAndPort applyIconName) (argumentPorts applyNode)
    functionPort = nameAndPort applyIconName (inputPort applyNode)
    combinedGraph = combineExpressions inPattern
                    $ zip (funVal:argVals) (functionPort:argumentNamePorts)
    icons = [Named applyIconName (mkEmbedder applyNode)]
    newGraph = syntaxGraphFromNodes $ Set.fromList icons

-- BEGIN BEGIN BEGIN BEGIN BEGIN evalPattern
makePatternGraph :: Show l =>
  EvalContext
  -> SimpPat l
  -> SimpExp l
  -> State IDState ((GraphAndRef, Maybe String), GraphAndRef)
makePatternGraph c pat e = do
  patGraphAndRef <- evalPattern pat
  let
    rhsContext = Set.union (namesInPattern patGraphAndRef) c
  rhsGraphAndRef <- evalExp rhsContext e
  pure (patGraphAndRef, rhsGraphAndRef)
-- TODO use in listComp
evalPatternApp :: Show l =>
  String
  -> [SimpPat l]
  -> State IDState (SyntaxGraph, NameAndPort)
evalPatternApp constructorName patterns = case patterns of
  [] -> makeBox constructorName
  _ ->  do
    patName <- getUniqueName
    evaledPatterns <- mapM evalPattern patterns
    pure $ makeNestedPatternGraph patName constructorName evaledPatterns

evalPatternLit ::
  Exts.Sign l -> Exts.Literal l -> State IDState (SyntaxGraph, NameAndPort)
evalPatternLit sign lit = makeBox $ showLiteral sign lit
-- END evalPatternLit

evalPAsPat :: Show l =>
  String -> SimpPat l -> State IDState (GraphAndRef, Maybe String)
evalPAsPat outerName p = do
  (GraphAndRef evaledPatGraph evaledPatRef, mInnerName) <- evalPattern p
  let
    asBindGraph = makeAsBindGraph (Left outerName) [mInnerName]
  pure (GraphAndRef (asBindGraph <> evaledPatGraph) evaledPatRef
       , Just outerName)

-- TODO add PatternValuePortConst to all
evalPattern :: Show l => SimpPat l -> State IDState (GraphAndRef, Maybe String)
evalPattern p = case p of
  SpVar _ _ -> pure (GraphAndRef mempty (Left $ simpPatNameStr p ), Nothing)
  SpLit _ sign lit -> makePatternResult $ evalPatternLit sign lit
  SpApp _ _ patterns -> makePatternResult $ evalPatternApp (simpPatNameStr p) patterns
  SpAsPat _ _ pat -> evalPAsPat (simpPatNameStr p) pat
  SpWildCard _ -> makePatternResult $ makeBox patternWildCardStr
  -- _ -> error ("evalPattern todo: " <> show p)

makeNestedPatternGraph ::
  NodeName
  -> String
  -> [(GraphAndRef, Maybe String)]
  -> (SyntaxGraph, NameAndPort)
makeNestedPatternGraph applyIconName funStr argVals = nestedApplyResult
  where
    dummyNode = PatternApplyNode defaultPatternNameStr []

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

    pAppNode = PatternApplyNode funStr argList
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
strToGraphRef :: EvalContext -> String -> State IDState GraphAndRef
strToGraphRef c str = fmap mapper (makeBox str) where
  mapper gr = if Set.member str c
    then GraphAndRef mempty (Left str)
    else grNamePortToGrRef gr

makePatternResult :: Functor f =>
  f (SyntaxGraph, NameAndPort) -> f (GraphAndRef, Maybe String)
makePatternResult
  = fmap (\(graph, namePort) -> (GraphAndRef graph (Right namePort), Nothing))

-- END END END END END evalPattern

-- BEGIN BEGIN BEGIN BEGIN BEGIN list comp

-- valus form ListCompNode are connected to item constructor
-- TODO reconsider PORT architecture choise to identfy arguments
-- TODO improve connection to guard expresion
-- TODO connect valus from generators to ListCompNode 
evalListComp :: Show l =>
  EvalContext -> l -> SimpExp l -> [SimpQStmt l] -> State IDState GraphAndRef
evalListComp context l  itemExp qualExps =  do  
  
  let decls = [d | (SqLet _l d ) <- qualExps]
  declGraphAndRefdeclContexts <-  mapM (evalDecls context) decls -- TODO add decls to context and graph
  let (declGraphAndRef, declContexts) = unzip declGraphAndRefdeclContexts
  let declContext =  Set.unions (context : declContexts)
  

  let gens  = [x | x@(SqGen {}) <- qualExps]
  genGRContextsAndGRpatRef <- mapM (evalSqGen declContext)  gens
  let genGraphRefsAndContexts = fmap fst genGRContextsAndGRpatRef
  let genGraphsAndRefs =  fmap snd genGRContextsAndGRpatRef
  let genContext = Set.unions (declContext : (fmap namesInPattern genGraphRefsAndContexts))

  listCompItemGraphAndRef@(GraphAndRef _ listCompItemRef)  <- evalExp genContext itemExp
  let declGraphsAndRefs = fmap (getRefForListCompItem listCompItemRef) declGraphAndRef

  let quals = [q | (SqQual _l q) <- qualExps]
  qualsGraphsAndRefs <- mapM (evalExp genContext) quals

  listCompName <- getUniqueName
  let listCompNode = ListCompNode
  let listCompNodeRef = nameAndPort listCompName (resultPort listCompNode)

  let expGraphsAndRefs = qualsGraphsAndRefs ++ declGraphsAndRefs

  let combinedGraph = makeListCompGraph context listCompNode listCompNodeRef listCompItemGraphAndRef expGraphsAndRefs genGraphsAndRefs

  pure (GraphAndRef combinedGraph (Right listCompNodeRef))

getRefForListCompItem :: Reference -> SyntaxGraph -> GraphAndRef
getRefForListCompItem expResultRef bindGraph = GraphAndRef bindGraph ref where
  bindings = sgBinds bindGraph
  ref = lookupReference bindings expResultRef

-- evalSqGen :: EvalContext
--                -> SimpQStmt l
--                -> (State IDState GraphAndRef, Maybe String)
evalSqGen context (SqGen _l values itemPat) = do 
  (GraphAndRef patternGraph patternRef, itemContext) <- evalPattern itemPat
  (GraphAndRef valueGraph valueRef) <- evalExp context values -- TODO use "pat" to put value into item constructor
  let graph = valueGraph <> patternGraph
  pure ((GraphAndRef graph patternRef, itemContext),(GraphInPatternRef graph valueRef patternRef))

evalSqGen _ _ = error "SimpQStmt must be SqGen" 

makeListCompGraph :: EvalContext -> SyntaxNode -> NameAndPort
                       -> GraphAndRef -> [GraphAndRef] -> [GraphInPatternRef]-> SyntaxGraph
makeListCompGraph context listCompNode listCompNodeRef listCompItemGraphAndRef qualGraphsAndRefs genGraphsAndRefs 
  = combinedGraph where
  (NameAndPort listCompName _) = listCompNodeRef

  qualGraphs = mconcat $ fmap  graphAndRefToGraph qualGraphsAndRefs
  genGraphs = mconcat $ fmap  graphInPatternRefToGraph genGraphsAndRefs

  qStmtGraphs = qualGraphs <> genGraphs

  listCompItemGraph = {-makeEdges context $-} combineExpresionsIsSource makeSimpleEdge
    (listCompItemGraphAndRef, NameAndPort listCompName (Just (inputPort listCompNode)))

  listCompNodeGraph = syntaxGraphFromNodes
    $ Set.singleton (Named listCompName (mkEmbedder listCompNode))

  innerOutputGraph = makeInnerOutputEdges listCompName 
    (qualGraphsAndRefs ++ fmap graphInPatternRefToGraphAndPat genGraphsAndRefs)

  innerInputGraph = makeInnerInputEdges listCompName 
    (qualGraphsAndRefs ++ fmap graphInPatternRefToGraphAndRef genGraphsAndRefs)

  combinedGraph = makeEdges context
    (qStmtGraphs <> listCompItemGraph  <> listCompNodeGraph <> innerOutputGraph <> innerInputGraph) -- <> outputGraph

makeInnerOutputEdges :: NodeName -> [GraphAndRef] -> SyntaxGraph
makeInnerOutputEdges listCompName graphsAndRefs = graph where
  listCompPorts = map (nameAndPort listCompName) resultPortsConst
  binds = catMaybes $ zipWith makeInnerOutputBind graphsAndRefs listCompPorts
  graph = bindsToSyntaxGraph ( SMap.fromList binds)

makeInnerOutputBind :: GraphAndRef -> NameAndPort -> Maybe SgBind
makeInnerOutputBind (GraphAndRef _ ref) lamPort = case ref of
  Left str -> Just (str, Right lamPort)
  Right _ -> Nothing
  

makeInnerInputEdges :: NodeName -> [GraphAndRef] -> SyntaxGraph
makeInnerInputEdges listCompName graphsAndRefs = graph where
  listCompPorts = map (nameAndPort listCompName) argPortsConst
  edges =  catMaybes $ zipWith makeInnerInputEdge graphsAndRefs listCompPorts
  graph = syntaxGraphFromEdges ( Set.fromList edges)

makeInnerInputEdge :: GraphAndRef -> NameAndPort -> Maybe Edge
makeInnerInputEdge (GraphAndRef _ ref) listCompPort = case ref of
  Left _ ->  Nothing
  Right port -> Just $ makeSimpleEdge (port, listCompPort)


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
evalPatBind :: Show l =>
  l -> EvalContext -> SimpPat l -> SimpExp l -> State IDState GraphAndRef
evalPatBind _ c pat e = do
  ((GraphAndRef patGraph patRef, mPatAsName), GraphAndRef rhsGraph rhsRef) <-
    makePatternGraph c pat e
  let
    (newEdges, newSinks, bindings) = evalPatBindHelper patRef rhsRef
    asBindGraph = makeAsBindGraph rhsRef [mPatAsName]
    gr = asBindGraph <> SyntaxGraph mempty newEdges newSinks bindings mempty
    combinedGraph = gr <> rhsGraph <> patGraph
  pure (GraphAndRef (makeEdgesKeepBindings c combinedGraph) patRef)

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
evalTypeSig :: Show l =>
  [Exts.Name l] -> Exts.Type l
  -> State IDState (SyntaxGraph, NameAndPort)
evalTypeSig names typeForNames = makeBox
  (intercalate typeNameSeparatorStr (fmap prettyPrintWithoutNewlines names)
   ++ typeSignatureSeparatorStr
   ++ prettyPrintWithoutNewlines typeForNames)
  where
    -- TODO Make custom version of prettyPrint for type signitures.
    -- Use (unwords . words) to convert consecutive whitspace characters to one
    -- space.
    prettyPrintWithoutNewlines = unwords . words . Exts.prettyPrint

evalDecl :: Show l => EvalContext -> SimpDecl l -> State IDState GraphAndRef
evalDecl c d = case d of
  SdPatBind l pat e -> evalPatBind l c pat e
  _ -> error "TODO reference for inner graphs"
  -- SdTypeSig _ names typeForNames -> fst <$> evalTypeSig names typeForNames
  -- SdCatchAll decl -> fst <$> makeBox (PExts.prettyPrint decl)

evalTopDecl :: Show l => EvalContext -> SimpDecl l -> State IDState SyntaxGraph
evalTopDecl c d = case d of
  SdPatBind l pat e -> showTopLevelBind l c pat e
  SdTypeSig _ names typeForNames -> fst <$> evalTypeSig names typeForNames
  SdCatchAll decl -> fst <$> makeBox (PExts.prettyPrint decl)

-- END END END END END evalDecl

-- TODO improve this
-- showTopLevelBind ::Show l => SimpPat l -> State IDState SyntaxGraph
showTopLevelBind :: Show l 
  => l
  -> EvalContext
  -> SimpPat l
  -> SimpExp l
  -> State IDState SyntaxGraph
showTopLevelBind  l c pat e = do
  GraphAndRef gr originalRef <- evalPatBind l c pat e
  let ref = lookupReference (sgBinds gr) originalRef
  case ref of 
    Left str -> do
      pure (gr)
    Right np -> do
      uniquePatName <- getUniqueName
      let
        patName = simpPatNameStr pat
        icons = Set.singleton (Named uniquePatName $ mkEmbedder (BindNameNode patName))
        edges = Set.singleton (makeSimpleEdge (np, justName uniquePatName))
        bindGraph = syntaxGraphFromNodesEdges icons edges
      pure (bindGraph <> gr)


translateDeclToSyntaxGraph :: Show l => SimpDecl l -> SyntaxGraph
translateDeclToSyntaxGraph d = deleteBindings graph where
  evaluatedDecl = evalTopDecl mempty d -- >>= showTopLevelBinds
  graph = evalState evaluatedDecl initialIdState

-- | Convert a single function declaration into a SyntaxGraph
-- Used in Unit Tests
translateStringToSyntaxGraph :: String -> SyntaxGraph
translateStringToSyntaxGraph = translateDeclToSyntaxGraph . stringToSimpDecl

-- END Exported functions
