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
import           Data.Either                    ( partitionEithers )
import           Data.Maybe( 
  catMaybes
  , listToMaybe
  , fromMaybe
  )
import qualified Language.Haskell.Exts as Exts
import qualified Language.Haskell.Exts.Pretty as PExts

import           PortConstants (
  inputPort
  , resultPort
  , argumentPorts
  , caseValuePorts
  , casePatternPorts
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
  )
import           Types(
  NameAndPort(..)
  , IDState
  , Edge
  , SyntaxNode(..)
  , NodeName(..)
  , LikeApplyFlavor(..)
  , CaseOrMultiIfTag(..)
  , Named(..)
  , mkEmbedder
  )
import Util(makeSimpleEdge,makeInvisibleEdge, nameAndPort, justName)
import SyntaxGraph( 
    SyntaxGraph(..)
  , patternName
  , syntaxGraphFromNodes
  , syntaxGraphFromNodesEdges
  , bindsToSyntaxGraph
  , graphAndRefToGraph
  , getUniqueName
  , getUniqueString
  , edgesForRefPortList
  , combineExpressions
  , makeApplyGraph
  , makeMultiIfGraph
  , namesInPattern
  , lookupReference
  , deleteBindings
  , makeEdgesAndDeleteBindings
  , makeEdges
  , makeBox
  , asBindGraphZipper
  , makePatternEdgeInLambda
  , GraphAndRef(..)
  , Reference(..)
  , SgSink(..)
  , SgBind
  , GraphInPatternRef(..)
  , strToGraphRef
  , grNamePortToGrRef
  , makeNestedPatternGraph
  , makeAsBindGraph
  , makePatternResult
  , initialIdState
  , makeListCompGraph
  , evalPatBindHelper
  , EvalContext
  -- , makeNotConstraintEdgesAndDeleteBindings
  )
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
-- SimpSyntaxToSyntaxGraph SimpExp into subgraf of type SyntaxGraph
-- Generated SyntaxGraph has sgNodes sgEdges sgSinks sgBinds sgEmbedMap
-- for one SimpExp
-- sgNodes has unik ID called Name
-- ARCHITECTURE NOTE --
-- The core functions and data types used in this module are in SyntaxNodeToIcon.
-- The SyntaxNodeToIcon also contains most/all of the translation functions that
-- do not use Language.Haskell.Exts.

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
  SeListComp l e eList -> evalListComp c l e eList


-- This is in SimpSyntaxToSyntaxGraph and not SimpSyntaxToSyntaxGraph core since currently it is only used
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

-- TODO add PatternValuePortConst to all
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

evalDecls :: Show l =>
  EvalContext -> [SimpDecl l] -> State IDState (SyntaxGraph, EvalContext)
evalDecls c decls =
  let
    boundNames = Set.unions (fmap getBoundVarName decls)
    augmentedContext = Set.union boundNames c
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
    newGraph = makeEdgesAndDeleteBindings $ expGraph <> bindGraph
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
    makePatternGraph c pat rhs
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

    finalGraph = makeEdgesAndDeleteBindings $ mconcat [bindGraph
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

-- BEGIN BEGIN BEGIN BEGIN BEGIN evalLambda 
evalLambda :: Show l
  => l
  -> EvalContext
  -> [SimpPat l]
  -> SimpExp l
  -> String
  -> State IDState (SyntaxGraph, NameAndPort)
evalLambda _ context patterns expr functionName = do
  lambdaName <- getUniqueName
  argNodeName <- getUniqueName
  patternValsWithAsNames <- mapM evalPattern patterns
  let
    patternStrings = Set.unions $ fmap namesInPattern patternValsWithAsNames
    rhsContext = Set.union patternStrings context
  GraphAndRef rhsRawGraph rhsRef <- evalExp rhsContext expr
  let
    patternVals = fmap fst patternValsWithAsNames
    argNode = makeLambdaArgumentNode patternValsWithAsNames
    lambdaNode = makeLambdaNode combinedGraph functionName lambdaName
    lambdaPorts = map (nameAndPort argNodeName) $ argumentPorts lambdaNode
    patternGraph = mconcat $ fmap graphAndRefToGraph patternVals

    outputNameAndPort = getOutputNameAndPort rhsRef argNodeName patternVals lambdaPorts
    lambdaValueEdge = makeSimpleEdge (outputNameAndPort, nameAndPort lambdaName (inputPort lambdaNode))
    lambdaArgAboveValue = makeInvisibleEdge (justName argNodeName, justName lambdaName)
    lambdaEdges = (lambdaValueEdge : lambdaArgAboveValue : patternEdges')

    (patternEdges', newBinds') =
      partitionEithers $ zipWith makePatternEdgeInLambda patternVals lambdaPorts

    lambdaIconAndOutputGraph
      = makeLambdaOutputGraph lambdaEdges newBinds' (lambdaName ,lambdaNode) (argNodeName,argNode)

    asBindGraph = mconcat $ zipWith
                  asBindGraphZipper
                  (fmap snd patternValsWithAsNames)
                  lambdaPorts
    combinedGraph = makeEdgesAndDeleteBindings
                    $ (asBindGraph <> rhsRawGraph <> patternGraph <> lambdaIconAndOutputGraph)

    resultNameAndPort = nameAndPort lambdaName (resultPort lambdaNode)
  pure (combinedGraph, resultNameAndPort)
  
    -- TODO Like evalPatBind, this edge should have an indicator that it is the
    -- input to a pattern.
    -- makePatternEdgeInLambda creates the edges between the patterns and the parameter
    -- ports.
-- add test \x y = (y, f x)
getOutputNameAndPort rhsRef argNodeName patternVals lambdaPorts = case rhsRef of
  strRef@(Left _) -> inputStraightToAutput where 
    maybeRefList = catMaybes $ zipWith (makeReferenceToArgument strRef) patternVals lambdaPorts
    returnPort = NameAndPort argNodeName Nothing
    inputStraightToAutput = fromMaybe returnPort $ listToMaybe maybeRefList
  (Right np) -> np

makeReferenceToArgument :: Reference -> GraphAndRef -> NameAndPort  -> Maybe NameAndPort
makeReferenceToArgument rhsRef (GraphAndRef _ ref) lamPort = if rhsRef == ref
      then Just lamPort
      else Nothing

makeLambdaNode :: SyntaxGraph -> String -> NodeName-> SyntaxNode
makeLambdaNode combinedGraph  functionName lambdaName = node where
  enclosedNodeNames =  Set.delete lambdaName
    $ Set.map naName (sgNodes combinedGraph)
  node = FunctionValueNode functionName enclosedNodeNames

makeLambdaArgumentNode :: [(GraphAndRef, Maybe String)] -> SyntaxNode
makeLambdaArgumentNode patternValsWithAsNames = node where 
  paramNames = fmap patternName patternValsWithAsNames
  node = FunctionArgNode paramNames


makeLambdaOutputGraph ::
  [Edge] 
  -> [(SMap.Key, Reference)] 
  -> (NodeName, SyntaxNode)
  -> (NodeName, SyntaxNode)
  -> SyntaxGraph
makeLambdaOutputGraph patternEdgesList binds (lambdaName ,lambdaNode) (argNodeName,argNode) = graph where
  patternEdges = Set.fromList patternEdgesList
  bindsSet = SMap.fromList binds
  lambdaIconSet = Set.fromList [
    (Named lambdaName (mkEmbedder lambdaNode))
    , (Named argNodeName (mkEmbedder argNode))
    ]
  graph = SyntaxGraph lambdaIconSet patternEdges mempty bindsSet mempty
-- END END END END END evalLambda 


-- valus form ListCompNode are connected to item constructor
-- TODO reconsider PORT architecture choise to identfy arguments
-- TODO improve connection to guard expresion
-- TODO connect valus from generators to ListCompNode 
evalListComp :: Show l =>
  EvalContext -> l -> SimpExp l -> [SimpQStmt l] -> State IDState GraphAndRef
evalListComp context l  itemExp qualExps =  do  
  
  let decls = [d | (SqLet _l d ) <- qualExps]
  declGraphRefsAndContexts <- mapM (evalDecls context) decls -- TODO add decls to context and graph
  let declContext =  Set.unions (context : (fmap snd  declGraphRefsAndContexts))

  let gens  = [x | x@(SqGen {}) <- qualExps]
  genGRContextsAndGRpatRef <- mapM (evalSqGen declContext)  gens
  let genGraphRefsAndContexts = fmap fst genGRContextsAndGRpatRef
  let genGraphsAndRefs =  fmap snd genGRContextsAndGRpatRef
  let genContext = Set.unions (declContext : (fmap namesInPattern genGraphRefsAndContexts))

  listCompItemGraphAndRef@(GraphAndRef _ listCompItemRef)  <- evalExp genContext itemExp
  let declGraphsAndRefs = fmap (getRefForListCompItem listCompItemRef) (fmap fst declGraphRefsAndContexts)

  let quals = [q | (SqQual _l q) <- qualExps]
  qualsGraphsAndRefs <- mapM (evalExp genContext) quals

  listCompName <- getUniqueName
  let listCompNode = ListCompNode
  let listCompNodeRef = nameAndPort listCompName (resultPort listCompNode)

  let expGraphsAndRefs = qualsGraphsAndRefs ++ declGraphsAndRefs

  let combinedGraph = makeListCompGraph listCompNode listCompNodeRef listCompItemGraphAndRef expGraphsAndRefs genGraphsAndRefs

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

-- TODO refactor with similar pattern functions
evalPatBind :: Show l =>
  l -> EvalContext -> SimpPat l -> SimpExp l -> State IDState SyntaxGraph
evalPatBind _ c pat e = do
  ((GraphAndRef patGraph patRef, mPatAsName), GraphAndRef rhsGraph rhsRef) <-
    makePatternGraph c pat e
  let
    (newEdges, newSinks, bindings) = evalPatBindHelper patRef rhsRef
    asBindGraph = makeAsBindGraph rhsRef [mPatAsName]
    gr = asBindGraph <> SyntaxGraph mempty newEdges newSinks bindings mempty
  pure . makeEdges $ (gr <> rhsGraph <> patGraph)

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

-- BEGIN Exported functions -- TODO move them up a level

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
-- Used in Unit Tests
translateStringToSyntaxGraph :: String -> SyntaxGraph
translateStringToSyntaxGraph = translateDeclToSyntaxGraph . stringToSimpDecl

-- END Exported functions
