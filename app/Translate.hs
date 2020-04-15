{-# LANGUAGE NoMonomorphismRestriction, TupleSections, PatternSynonyms #-}

module Translate(
  translateStringToSyntaxGraph,
  translateStringToCollapsedGraphAndDecl,
  translateModuleToCollapsedGraphs,
  customParseDecl
) where

import Diagrams.Prelude((<>))

import Control.Monad(replicateM)
import Control.Monad.State(State, evalState)
import Data.Either(partitionEithers)
import qualified Data.Graph.Inductive.PatriciaTree as FGR
import Data.List(unzip5, partition, intercalate)
import Data.Maybe(fromMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.StringMap as SMap
import qualified Data.IntMap as IMap

import qualified Language.Haskell.Exts as Exts
import qualified Language.Haskell.Exts.Pretty as PExts

import GraphAlgorithms(annotateGraph, collapseAnnotatedGraph)
import PortConstants(inputPort, resultPort, argumentPorts, caseValuePorts,
             casePatternPorts)
import SimplifySyntax(SimpAlt(..), stringToSimpDecl, SimpExp(..), SimpPat(..)
                     , qNameToString, nameToString, customParseDecl
                     , SimpDecl(..), hsDeclToSimpDecl, SelectorAndVal(..)
                     , pattern FunctionCompositionStr)
import TranslateCore(Reference, SyntaxGraph(..), EvalContext, GraphAndRef(..)
                    , SgSink(..), syntaxGraphFromNodes
                    , syntaxGraphFromNodesEdges, getUniqueName
                    , edgesForRefPortList, makeApplyGraph, makeMultiIfGraph
                    , combineExpressions, namesInPattern, lookupReference
                    , deleteBindings, makeEdges, makeBox, syntaxGraphToFglGraph
                    , getUniqueString, bindsToSyntaxGraph, SgBind(..)
                    , graphAndRefToGraph, initialIdState)
import Types(AnnotatedGraph, Labeled(..), NameAndPort(..), IDState,
             Edge, SyntaxNode(..), NodeName(..), SgNamedNode,
             LikeApplyFlavor(..), CaseOrMultiIfTag(..), Named(..)
            , mkEmbedder)
import Util(makeSimpleEdge, nameAndPort, justName)

import StringSymbols(
  listCompositionPlaceholderStr
  , typeSignatureSeparatorStr
  , typeNameSeparatorStr
  , negativeLiteralStr
  , patternWildCardStr
  , unusedArgumentStr
  , defaultPatternNameStr
  )

{-# ANN module "HLint: ignore Use record patterns" #-}

-- OVERVIEW --
-- Translate SimpExp into subgraf of type SyntaxGraph
-- Generated SyntaxGraph has sgNodes sgEdges sgSinks sgBinds sgEmbedMap
-- for one SimpExp
-- sgNodes has unik ID called Name
-- ARCHITECTURE NOTE --
-- The core functions and data types used in this module are in TranslateCore.
-- The TranslateCore also contains most/all of the translation functions that
-- do not use Language.Haskell.Exts.

-- BEGIN Helper Functions --

-- | Make a syntax graph that has the bindings for a list of "as pattern" (@)
-- names.
makeAsBindGraph :: Reference -> [Maybe String] -> SyntaxGraph
makeAsBindGraph ref asNames
  = bindsToSyntaxGraph (SMap.fromList (mapMaybe makeBind asNames))
  where
    makeBind mName = case mName of
      Nothing -> Nothing
      Just asName -> Just $ (asName, ref)

grNamePortToGrRef :: (SyntaxGraph, NameAndPort) -> GraphAndRef
grNamePortToGrRef (graph, np) = GraphAndRef graph (Right np)

-- TODO Find a better name for bindOrAltHelper
bindOrAltHelper :: Show l =>
  EvalContext
  -> SimpPat l
  -> SimpExp l
  -> State IDState ((GraphAndRef, Maybe String), GraphAndRef)
bindOrAltHelper c pat e = do
  patGraphAndRef <- evalPattern pat
  let
    rhsContext = namesInPattern patGraphAndRef <> c
  rhsGraphAndRef <- evalExp rhsContext e
  pure (patGraphAndRef, rhsGraphAndRef)


patternName :: (GraphAndRef, Maybe String) -> String
patternName (GraphAndRef _ ref, mStr) = fromMaybe
  (case ref of
    Left str -> str
    Right _ -> defaultPatternNameStr
  )
  mStr

-- END Helper Functions --

-- BEGIN evalLit

-- This is in Translate and not Translate core since currently it is only used
-- by evalLit.
makeLiteral :: (Show x) => x -> State IDState (SyntaxGraph, NameAndPort)
makeLiteral = makeBox . show

evalLit :: Exts.Literal l -> State IDState (SyntaxGraph, NameAndPort)
evalLit (Exts.Int _ x _) = makeLiteral x
evalLit (Exts.Char _ x _) = makeLiteral x
evalLit (Exts.String _ x _) = makeLiteral x
evalLit (Exts.Frac _ x _) = makeLiteral x
-- TODO: Test the unboxed literals
evalLit (Exts.PrimInt _ x _) = makeLiteral x
evalLit (Exts.PrimWord _ x _) = makeLiteral x
evalLit (Exts.PrimFloat _ x _) = makeLiteral x
evalLit (Exts.PrimDouble _ x _) = makeLiteral x
evalLit (Exts.PrimChar _ x _) = makeLiteral x
evalLit (Exts.PrimString _ x _) = makeLiteral x

-- END evalLit

-- BEGIN evalPattern

-- BEGIN evalPatternApp
asNameBind :: (GraphAndRef, Maybe String) -> Maybe SgBind
asNameBind (GraphAndRef _ ref, mAsName) = case mAsName of
  Nothing -> Nothing
  Just asName -> Just ( asName, ref)

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


graphToTuple ::
  SyntaxGraph
  -> (Set.Set SgNamedNode, (Set.Set Edge), (Set.Set SgSink), (SMap.StringMap Reference), IMap.IntMap NodeName)
graphToTuple (SyntaxGraph a b c d e) = (a, b, c, d, e)

graphsToComponents ::
  [SyntaxGraph]
  -> (Set.Set SgNamedNode, (Set.Set Edge), (Set.Set SgSink), (SMap.StringMap Reference), IMap.IntMap NodeName)
graphsToComponents graphs = graphToTuple $ mconcat graphs

  -- = (mconcat a, mconcat b, mconcat c, mconcat d, mconcat e)
  -- where
  --   (a, b, c, d, e) = unzip5 $ fmap graphToTuple graphs

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


evalPatternApp :: Show l =>
  Exts.QName l
  -> [SimpPat l]
  -> State IDState (SyntaxGraph, NameAndPort)
evalPatternApp name patterns = case patterns of
  [] -> makeBox constructorName
  _ ->  do
    patName <- getUniqueName
    evaledPatterns <- mapM evalPattern patterns
    pure $ makeNestedPatternGraph patName constructorName evaledPatterns
  where
    constructorName = qNameToString name

-- END evalPatternApp

-- BEGIN evalPatternLit
showLiteral :: Exts.Literal l -> String
showLiteral (Exts.Int _ x _) = show x
showLiteral (Exts.Char _ x _) = show x
showLiteral (Exts.String _ x _) = show x
-- TODO: Print the Rational as a floating point.
showLiteral (Exts.Frac _ x _) = show x
-- TODO: Test the unboxed literals
showLiteral (Exts.PrimInt _ x _) = show x
showLiteral (Exts.PrimWord _ x _) = show x
showLiteral (Exts.PrimFloat _ x _) = show x
showLiteral (Exts.PrimDouble _ x _) = show x
showLiteral (Exts.PrimChar _ x _) = show x
showLiteral (Exts.PrimString _ x _) = show x

evalPatternLit ::
  Exts.Sign l -> Exts.Literal l -> State IDState (SyntaxGraph, NameAndPort)
evalPatternLit sign l = case sign of
  Exts.Signless _ -> evalLit l
  Exts.Negative _ -> makeBox (negativeLiteralStr ++ showLiteral l)
-- END evalPatternLit

evalPAsPat :: Show l =>
  Exts.Name l -> SimpPat l -> State IDState (GraphAndRef, Maybe String)
evalPAsPat n p = do
  (GraphAndRef evaledPatGraph evaledPatRef, mInnerName) <- evalPattern p
  let
    outerName = nameToString n
    asBindGraph = makeAsBindGraph (Left outerName) [mInnerName]
  pure (GraphAndRef (asBindGraph <> evaledPatGraph) evaledPatRef
       , Just outerName)

makePatternResult :: Functor f =>
  f (SyntaxGraph, NameAndPort) -> f (GraphAndRef, Maybe String)
makePatternResult
  = fmap (\(graph, namePort) -> (GraphAndRef graph (Right namePort), Nothing))

evalPattern :: Show l => SimpPat l -> State IDState (GraphAndRef, Maybe String)
evalPattern p = case p of
  SpVar _ n -> pure (GraphAndRef mempty (Left $ nameToString n), Nothing)
  SpLit _ sign lit -> makePatternResult $ evalPatternLit sign lit
  SpApp _ name patterns -> makePatternResult $ evalPatternApp name patterns
  SpAsPat _ name pat -> evalPAsPat name pat
  SpWildCard _ -> makePatternResult $ makeBox patternWildCardStr
  -- _ -> error ("evalPattern todo: " <> show p)

-- END evalPattern

-- BEGIN evalQName

-- strToGraphRef is not in TranslateCore, since it is only used by evalQName.
strToGraphRef :: EvalContext -> String -> State IDState GraphAndRef
strToGraphRef c str = fmap mapper (makeBox str) where
  mapper gr = if str `elem` c
    then GraphAndRef mempty (Left str)
    else grNamePortToGrRef gr

-- END evalQName

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

-- Todo add test for this function
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

getBoundVarName :: Show l => SimpDecl l -> [String]
getBoundVarName d = case d of
  SdPatBind _ pat _ -> namesInPattern
                     -- TODO Should evalState be used here?
                     $ evalState (evalPattern pat) initialIdState
  SdTypeSig _ _ _ -> []
  SdCatchAll _ -> []

evalDecls :: Show l =>
  EvalContext -> [SimpDecl l] -> State IDState (SyntaxGraph, EvalContext)
evalDecls c decls =
  let
    boundNames = concatMap getBoundVarName decls
    augmentedContext = boundNames <> c
  in
    (,augmentedContext) . mconcat <$> mapM (evalDecl augmentedContext) decls

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
    newGraph = deleteBindings . makeEdges $ expGraph <> bindGraph
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

-- BEGIN evalCase

-- TODO patRhsAreConnected is sometimes incorrectly true if the pat is just a
-- name
-- returns (combined graph, pattern reference, rhs reference)
evalAlt :: Show l =>
  EvalContext
  -> SimpAlt l
  -> State IDState (Bool, SyntaxGraph, Reference, Reference, Maybe String)
evalAlt c (SimpAlt pat rhs) = do
  ((GraphAndRef patGraph patRef, mPatAsName), GraphAndRef rhsGraph rhsRef) <-
    bindOrAltHelper c pat rhs
  let
    grWithEdges = makeEdges (rhsGraph <> patGraph)
    lookedUpRhsRef = lookupReference (sgBinds grWithEdges) rhsRef
    -- The pattern and rhs are conneted if makeEdges added extra edges, or if
    -- the rhsRef refers to a source in the pattern.
    patRhsAreConnected
      = (rhsRef /= lookedUpRhsRef)
        || ( length (sgEdges grWithEdges)
             >
             (length (sgEdges rhsGraph) + length (sgEdges patGraph)))
  pure (patRhsAreConnected
       , deleteBindings grWithEdges
       , patRef
       , lookedUpRhsRef
       , mPatAsName)

evalCaseHelper ::
  Int
  -> NodeName
  -> [NodeName]
  -> GraphAndRef
  -> [(Bool, SyntaxGraph, Reference, Reference, Maybe String)]
  -> (SyntaxGraph, NameAndPort)
evalCaseHelper numAlts caseIconName resultIconNames
  (GraphAndRef expGraph expRef) evaledAlts
  = result
  where
    (patRhsConnected, altGraphs, patRefs, rhsRefs, asNames) = unzip5 evaledAlts
    combindedAltGraph = mconcat altGraphs
    caseNode = CaseOrMultiIfNode CaseTag numAlts
    icons = Set.singleton (Named caseIconName (mkEmbedder caseNode))
    caseGraph = syntaxGraphFromNodes icons
    expEdge = (expRef, nameAndPort caseIconName (inputPort caseNode))
    patEdges = zip patRefs $ map (nameAndPort caseIconName) casePatternPorts
    rhsEdges = zip patRhsConnected $ zip rhsRefs
               $ map (nameAndPort caseIconName) caseValuePorts
    (connectedRhss, unConnectedRhss) = partition fst rhsEdges

    makeCaseResult :: NodeName -> Reference -> SyntaxGraph
    makeCaseResult resultIconName rhsRef = case rhsRef of
      Left _ -> mempty
      Right rhsPort -> syntaxGraphFromNodesEdges rhsNewIcons rhsNewEdges
        where
          rhsNewIcons = Set.singleton (Named resultIconName (mkEmbedder CaseResultNode))
          rhsNewEdges = Set.singleton (makeSimpleEdge (rhsPort, justName resultIconName))

    caseResultGraphs =
      mconcat
      $ zipWith makeCaseResult resultIconNames (fmap (fst . snd) connectedRhss)
    filteredRhsEdges = fmap snd unConnectedRhss
    patternEdgesGraph = edgesForRefPortList True patEdges
    caseEdgeGraph = edgesForRefPortList False (expEdge : filteredRhsEdges)

    bindGraph = makeAsBindGraph expRef asNames

    finalGraph = deleteBindings $ makeEdges $ mconcat [bindGraph
                                                      , patternEdgesGraph
                                                      , caseResultGraphs
                                                      , expGraph
                                                      , caseEdgeGraph
                                                      , caseGraph
                                                      , combindedAltGraph]
    result = (finalGraph, nameAndPort caseIconName (resultPort caseNode))


evalCase :: Show l =>
  EvalContext -> SimpExp l -> [SimpAlt l]
  -> State IDState (SyntaxGraph, NameAndPort)
evalCase c e alts =
  let
    numAlts = length alts
  in
    evalCaseHelper (length alts)
    <$>
    getUniqueName
    <*>
    replicateM numAlts getUniqueName
    <*>
    evalExp c e
    <*>
    mapM (evalAlt c) alts

-- END evalCase

-- BEGIN generalEvalLambda

-- TODO Returning a SyntaxGraph is probably not very efficient
asBindGraphZipper :: Maybe String -> NameAndPort -> SyntaxGraph
asBindGraphZipper asName nameNPort = makeAsBindGraph (Right nameNPort) [asName]

-- TODO Refactor evalLambda
evalLambda :: Show l
  => l
  -> EvalContext
  -> [SimpPat l]
  -> SimpExp l
  -> String
  -> State IDState (SyntaxGraph, NameAndPort)
evalLambda _ context patterns expr functionName = do
  lambdaName <- getUniqueName
  patternValsWithAsNames <- mapM evalPattern patterns
  let
    patternVals = fmap fst patternValsWithAsNames
    patternStrings = concatMap namesInPattern patternValsWithAsNames
    rhsContext = patternStrings <> context
  GraphAndRef rhsRawGraph rhsRef <- evalExp rhsContext expr
  let
    paramNames = fmap patternName patternValsWithAsNames
    enclosedNodeNames =  Set.map naName (sgNodes combinedGraph)
    lambdaNode = FunctionDefNode paramNames functionName enclosedNodeNames
    lambdaPorts = map (nameAndPort lambdaName) $ argumentPorts lambdaNode
    patternGraph = mconcat $ fmap graphAndRefToGraph patternVals

    (patternEdges', newBinds') =
      partitionEithers $ zipWith makePatternEdges patternVals lambdaPorts

    patternEdges = Set.fromList patternEdges'
    newBinds = SMap.fromList newBinds'
    icons = Set.singleton (Named lambdaName (mkEmbedder lambdaNode))
    returnPort = nameAndPort lambdaName (inputPort lambdaNode)
    (newEdges, newSinks) = case rhsRef of
      Left s -> (patternEdges, Set.singleton (SgSink s returnPort))
      Right rhsPort ->
        (Set.insert (makeSimpleEdge (rhsPort, returnPort)) patternEdges, mempty)
    finalGraph = SyntaxGraph icons newEdges newSinks newBinds mempty

    asBindGraph = mconcat $ zipWith
                  asBindGraphZipper
                  (fmap snd patternValsWithAsNames)
                  lambdaPorts
    combinedGraph = deleteBindings . makeEdges
                    $ (asBindGraph <> rhsRawGraph <> patternGraph <> finalGraph)

  pure (combinedGraph, nameAndPort lambdaName (resultPort lambdaNode))
  where
    -- TODO Like evalPatBind, this edge should have an indicator that it is the
    -- input to a pattern.
    -- makePatternEdges creates the edges between the patterns and the parameter
    -- ports.
    makePatternEdges :: GraphAndRef -> NameAndPort -> Either Edge SgBind
    makePatternEdges (GraphAndRef _ ref) lamPort = case ref of
      Right patPort -> Left $ makeSimpleEdge (lamPort, patPort)
      Left str -> Right $ (str, (Right lamPort))

-- END generalEvalLambda

evalExp :: Show l => EvalContext -> SimpExp l -> State IDState GraphAndRef
evalExp c x = case x of
  SeName _ s -> strToGraphRef c s
  SeLit _ lit -> grNamePortToGrRef <$> evalLit lit
  SeApp _ _ _ -> grNamePortToGrRef <$> evalApp c x
  SeLambda l patterns e functionName-> grNamePortToGrRef <$> evalLambda l c patterns e functionName
  SeLet _ decls expr -> evalLet c decls expr
  SeCase _ expr alts -> grNamePortToGrRef <$> evalCase c expr alts
  SeMultiIf _ selectorsAndVals
    -> grNamePortToGrRef <$> evalMultiIf c selectorsAndVals
  SeListComp l e1 e2 -> evalListComp c l e1 e2

-- TODO make list composition work
evalListComp :: Show l =>
  EvalContext -> l -> SimpExp l -> Maybe (SimpExp l) -> State IDState GraphAndRef
evalListComp bindContext l  e1 Nothing =  do
  listCompName <- getUniqueName
  let node = LiteralNode listCompositionPlaceholderStr
  graph <- getGraphEvalListComp bindContext l  e1 node listCompName

  pure (GraphAndRef graph  (Right( nameAndPort listCompName (resultPort node)))  )

evalListComp bindContext l  e1 (Just e2) =  do
  listCompName <- getUniqueName
  let node = LiteralNode listCompositionPlaceholderStr
  graph <- getGraphEvalListComp bindContext l  e1 node listCompName
  GraphAndRef rhsRawGraph2 rhsRef2 <- evalExp bindContext e2
  let graph2 = graph <> rhsRawGraph2
  pure (GraphAndRef graph2  (Right( nameAndPort listCompName (resultPort node)))  )


getGraphEvalListComp bindContext l  e1 node  listCompName = do
    GraphAndRef rhsRawGraph1 rhsRef1 <- evalExp bindContext e1  
    let graph = syntaxGraphFromNodes $ Set.singleton $ Named listCompName (mkEmbedder node)
    pure $ graph <> rhsRawGraph1

-- getBasicGraph :: Show l 
--   => EvalContext -> l -> SimpExp l-> SyntaxNode -> NodeName
--   -> SyntaxGraph

  -- pure (finalGraph, nameAndPort listCompName (resultPort lambdaNode))
  
  -- graph2 = case  maybeE2 of 
  --   Just e2 -> graph <> rhsRawGraph1 <> rhsRawGraph2 
  --     GraphAndRef rhsRawGraph2 rhsRef2 <- evalExp bindContext e2
  --   _ -> graph <> rhsRawGraph1
    
evalPatBind :: Show l =>
  l -> EvalContext -> SimpPat l -> SimpExp l -> State IDState SyntaxGraph
evalPatBind _ c pat e = do
  ((GraphAndRef patGraph patRef, mPatAsName), GraphAndRef rhsGraph rhsRef) <-
    bindOrAltHelper c pat e
  let
    (newEdges, newSinks, bindings) = evalPatBindHelper patRef rhsRef
    asBindGraph = makeAsBindGraph rhsRef [mPatAsName]
    gr = asBindGraph <> SyntaxGraph mempty newEdges newSinks bindings mempty
  pure . makeEdges $ (gr <> rhsGraph <> patGraph)

evalPatBindHelper :: Reference -> Reference -> (Set.Set Edge, Set.Set SgSink, SMap.StringMap Reference)
evalPatBindHelper patRef rhsRef = case patRef of
  (Left s) -> (mempty, mempty, SMap.singleton s rhsRef)
  (Right patPort) -> case rhsRef of
    (Left rhsStr) -> (mempty, Set.singleton (SgSink rhsStr patPort), SMap.empty)
    (Right rhsPort) -> (Set.singleton (makeSimpleEdge (rhsPort, patPort)), mempty, SMap.empty)

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

evalDecl :: Show l => EvalContext -> SimpDecl l -> State IDState SyntaxGraph
evalDecl c d = case d of
  SdPatBind l pat e -> evalPatBind l c pat e
  SdTypeSig _ names typeForNames -> fst <$> evalTypeSig names typeForNames
  SdCatchAll decl -> fst <$> makeBox (PExts.prettyPrint decl)

-- END evalDecl

-- BEGIN Exported functions

showTopLevelBinds :: SyntaxGraph -> State IDState SyntaxGraph
showTopLevelBinds gr = do
  let
    binds = sgBinds gr
    addBind :: (String, Reference) -> State IDState SyntaxGraph
    addBind (_, (Left _)) = pure mempty
    addBind (patName, (Right port)) = do
      uniquePatName <- getUniqueName
      let
        icons = Set.singleton (Named uniquePatName $ mkEmbedder (BindNameNode patName))
        edges = Set.singleton (makeSimpleEdge (port, justName uniquePatName))
        edgeGraph = syntaxGraphFromNodesEdges icons edges
      pure edgeGraph
  newGraph <- mconcat <$> mapM addBind (SMap.toList binds)
  pure $ newGraph <> gr

translateDeclToSyntaxGraph :: Show l => SimpDecl l -> SyntaxGraph
translateDeclToSyntaxGraph d = graph where
  evaluatedDecl = evalDecl mempty d >>= showTopLevelBinds
  graph = evalState evaluatedDecl initialIdState

-- | Convert a single function declaration into a SyntaxGraph
translateStringToSyntaxGraph :: String -> SyntaxGraph
translateStringToSyntaxGraph = translateDeclToSyntaxGraph . stringToSimpDecl

syntaxGraphToCollapsedGraph :: SyntaxGraph -> AnnotatedGraph FGR.Gr
syntaxGraphToCollapsedGraph
  = collapseAnnotatedGraph . annotateGraph . syntaxGraphToFglGraph
  -- = annotateGraph . syntaxGraphToFglGraph

translateDeclToCollapsedGraph :: Show l => Exts.Decl l -> AnnotatedGraph FGR.Gr
translateDeclToCollapsedGraph
  = syntaxGraphToCollapsedGraph . translateDeclToSyntaxGraph . hsDeclToSimpDecl

-- Profiling: At one point, this was about 1.5% of total time.
translateStringToCollapsedGraphAndDecl ::
  String -> (AnnotatedGraph FGR.Gr, Exts.Decl Exts.SrcSpanInfo)
translateStringToCollapsedGraphAndDecl s = (drawing, decl) where
  decl = customParseDecl s -- :: ParseResult Module
  drawing = translateDeclToCollapsedGraph decl

translateModuleToCollapsedGraphs :: Show l =>
  Exts.Module l -> [AnnotatedGraph FGR.Gr]
translateModuleToCollapsedGraphs (Exts.Module _ _ _ _ decls)
  = fmap translateDeclToCollapsedGraph decls
translateModuleToCollapsedGraphs moduleSyntax
  = error $ "Unsupported syntax in translateModuleToCollapsedGraphs: "
    <> show moduleSyntax

-- END Exported functions
