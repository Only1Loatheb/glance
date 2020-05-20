{-# LANGUAGE PatternSynonyms #-}
module HsSyntaxToSimpSyntax (
  SimpExp(..)
  , SelectorAndVal(..)
  , SimpAlt(..)
  , SimpDecl(..)
  , SimpPat(..)
  , SimpQStmt(..)
  , stringToSimpDecl
  , qNameToString
  , nameToString
  , customParseDecl
  , hsDeclToSimpDecl
  , formatString
  , pattern FunctionCompositionStr
  , simpPatNameStr
  ) where

import Data.List(foldl')
import Data.Maybe(catMaybes, isJust, fromMaybe)

import qualified Language.Haskell.Exts as Exts

import StringSymbols(
  qualifiedSeparatorStr
  , unitConstructorStr
  , listTypeConstructorStr
  , functionConstructor
  , listDataConstructorStr
  , unboxedTupleConstructorStr
  , tempVarPrefix
  , otherwiseExpStr
  , negateSymbolStr
  , enumFromStr
  , enumFromToStr
  , enumFromThenStr
  , enumFromThenToStr
  , thenOperatorStr
  , bindOperatorStr
  , actionOverParameterizedType
  , nTupleString
  , nTupleSectionString
  , nListString
  , showLiteral
  , nameToString
  , qNameToString
  )

-- TODO use a data constructor for the special case instead of using string
-- matching for tempvars.
-- There is a special case in Icons.hs/makeLabelledPort to exclude " tempvar"

-- A simplified Haskell syntax tree

-- rhs is now SimpExp

-- A simplified Haskell expression.
data SimpExp l =
    SeName 
    { lSeName :: l
    , nameStr :: String }
  | SeLit
    { lSeLit  :: l
    , literal :: Exts.Literal l}
  | SeApp
    { lSeApp  :: l
    , function :: SimpExp l
    , argument :: SimpExp l}
  | SeLambda
    { lSeLambda  :: l
    , patAplications :: [SimpPat l]
    , bodyExpresion :: SimpExp l
    , laNameStr :: (Maybe String)}
  | SeLet
    { lSeLet  :: l
    , declarations :: [SimpDecl l]
    , bodyExpresion :: SimpExp l}
  | SeCase
    { lSeCase  :: l
    , bodyExpresion :: SimpExp l
    , alternatives :: [SimpAlt l]}
  | SeMultiIf
    { lSeMultiIf  :: l
    , selectorAndVal :: [SelectorAndVal l] }
  | SeListComp
    { lSeListComp  :: l
    , bodyExpresion :: SimpExp l
    , qualifyingExpresion ::[SimpQStmt l]} 
  deriving (Show, Eq)

data SimpStmt l = SimpStmt l String deriving (Show, Eq)

data SelectorAndVal l = SelectorAndVal
  { svSelector :: SimpExp l
  , svVal :: SimpExp l
  }
  deriving (Show, Eq)

data SimpAlt l = SimpAlt
  { saPat :: SimpPat l
  , saVal :: SimpExp l
  }
  deriving (Show, Eq)

data SimpDecl l =
  -- These don't have decl lists, since only lets have decl lists
  SdPatBind
    { lSdPatBind  :: l
    , patternBind :: SimpPat l
    , bindedExp :: SimpExp l}
  | SdTypeSig 
    { lSdTypeSig  :: l
    , typeSigNames :: [Exts.Name l]
    , typeBody :: Exts.Type l}
  -- TODO Add a visual representation of data declarations
  | SdCatchAll 
    { declaration :: Exts.Decl l}
  deriving (Show, Eq)

data SimpPat l =
  SpVar 
    { lSpVar  :: l
    ,varName :: Exts.Name l}
  | SpLit 
    { lSpLit  :: l
    , litSign :: Exts.Sign l
    , absValue :: Exts.Literal l}
  | SpApp 
    { lSpApp  :: l
    , appliedCons :: Exts.QName l
    , arguments :: [SimpPat l]}
  | SpAsPat 
    { lSpAsPat  :: l
    , alias :: Exts.Name l
    , aliasedPattern :: SimpPat l}
  | SpWildCard l
  deriving (Show, Eq)

data SimpQStmt l = 
  SqQual
    { lSqQual :: l
    , qualifier :: SimpExp l }
  | SqGen
    { lSqGen :: l 
    , valueGenerator :: SimpExp l
    , itemPattern :: SimpPat l}
  | SqLet 
    { lSqLet :: l
    , declarationBinds :: [SimpDecl l]}
  deriving (Show, Eq)
  
-- Helper functions

nameToQname :: Show l => l -> Exts.Name l -> Exts.QName l
nameToQname l name = strToQName l $ nameToString name  

strToQName :: l -> String -> Exts.QName l
strToQName l = Exts.UnQual l . Exts.Ident l

makeVarExp :: l -> String -> Exts.Exp l
makeVarExp l = Exts.Var l . strToQName l

makePatVar :: l -> String -> Exts.Pat l
makePatVar l = Exts.PVar l . Exts.Ident l

qOpToExp :: Exts.QOp l -> Exts.Exp l
qOpToExp (Exts.QVarOp l n) = Exts.Var l n
qOpToExp (Exts.QConOp l n) = Exts.Con l n

simpPatNameStr ::  Show l => SimpPat l -> String
simpPatNameStr (SpVar _ n)     = nameToString n
simpPatNameStr (SpLit _ sign lit) = showLiteral sign lit
simpPatNameStr (SpApp _ qn _)  = qNameToString qn
simpPatNameStr (SpAsPat _ n _) = nameToString n

simpExpToRhs :: Show l => l -> SimpExp l -> Exts.Rhs l
simpExpToRhs l e = Exts.UnGuardedRhs l (simpExpToHsExp e)

--

hsPatToSimpPat :: Show a => Exts.Pat a -> SimpPat a
hsPatToSimpPat p = case p of
  Exts.PVar l n -> SpVar l n
  Exts.PLit l sign lit -> SpLit l sign lit
  Exts.PInfixApp l p1 qName p2 -> hsPatToSimpPat (Exts.PApp l qName [p1, p2])
  Exts.PApp l name patts -> SpApp l name (fmap hsPatToSimpPat patts)
  Exts.PTuple l _ patts -> SpApp
                           l
                           ((strToQName l . nTupleString . length) patts)
                           (fmap hsPatToSimpPat patts)
  Exts.PParen _ pat -> hsPatToSimpPat pat
  Exts.PAsPat l name pat -> SpAsPat l name (hsPatToSimpPat pat)
  Exts.PWildCard l -> SpWildCard l
  Exts.PList l patts -> SpApp
                        l
                        ((strToQName l . nListString . length) patts)
                        (fmap hsPatToSimpPat patts)
  _ -> error $  "Unsupported syntax in hsPatToSimpPat: " <> show p

-- BEGIN BEGIN BEGIN BEGIN BEGIN matchesToFunBind
whereToLet :: Show a => a -> Exts.Rhs a -> Maybe (Exts.Binds a) -> SimpExp a
whereToLet l rhs maybeBinds = val
  where
    rhsExp = hsRhsToExp rhs
    val = case maybeBinds of
      Nothing -> rhsExp
      Just binds -> SeLet l (hsBindsToDecls binds) rhsExp

matchToDeclWPattern :: Show a => Exts.Match a -> SimpDecl a
matchToDeclWPattern (Exts.Match l name patterns rhs maybeWhereBinds)
  = SdPatBind l (SpApp l (nameToQname l name) simpPats) value where
    value = whereToLet l rhs maybeWhereBinds
    simpPats = fmap hsPatToSimpPat patterns
matchToDeclWPattern m = error $ "Unsupported syntax in matchToDeclWPattern: " <> show m

matchToDeclWLambda :: Show a => Exts.Match a -> SimpDecl a
matchToDeclWLambda (Exts.Match l name patterns rhs maybeWhereBinds)
  = SdPatBind l (SpVar l name) lambda where
    lambda = SeLambda l
      (fmap hsPatToSimpPat patterns)
      (whereToLet l rhs maybeWhereBinds)
      (Just $ nameToString name)
matchToDeclWLambda m = error $ "Unsupported syntax in matchToDeclWLambda: " <> show m

-- Only used by matchesToCase
matchToAlt :: Show l => Exts.Match l -> Exts.Alt l
matchToAlt (Exts.Match l _ mtaPats rhs binds)
  = Exts.Alt l altPattern rhs binds
  where
    altPattern = case mtaPats of
      [onePat] -> onePat
      _ -> Exts.PTuple l Exts.Boxed mtaPats
matchToAlt match = error $ "Unsupported syntax in matchToAlt: " <> show match
  
matchesToFunBind :: Show a => a -> [Exts.Match a] -> SimpDecl a
matchesToFunBind l [] = error $ "Empty matches in matchesToFunBind."
matchesToFunBind l [match] = matchToDeclWPattern match 
matchesToFunBind l allMatches@(firstMatch : _) = matchToDeclWLambda $ multipleMatchesToCase l firstMatch allMatches

-- TODO combine srcLoc
multipleMatchesToCase :: Show a => a -> Exts.Match a -> [Exts.Match a] -> Exts.Match a
multipleMatchesToCase srcLoc (Exts.Match _ funName pats _ _ ) allMatches
  = Exts.Match srcLoc funName casePats rhs Nothing where
    -- There is a special case in Icons.hs/makeLabelledPort to exclude " casevar"
    caseStrings = fmap (\x -> tempVarPrefix ++ show x) [0..(length pats - 1)]
    casePats = fmap (makePatVar srcLoc) caseStrings
    alts = fmap matchToAlt allMatches
    rhs = Exts.UnGuardedRhs srcLoc caseExp where 
      caseVars = fmap (makeVarExp srcLoc) caseStrings
      caseExp = case caseVars of
        [onecaseVar] -> Exts.Case srcLoc onecaseVar alts
        _ -> Exts.Case srcLoc tuple alts where
          tuple = Exts.Tuple srcLoc Exts.Boxed caseVars
multipleMatchesToCase l _ _ = error $ "Unsupported syntax in matchesToFunBind:" ++ show l
-- END END END END END matchesToFunBind

hsDeclToSimpDecl :: Show a => Exts.Decl a -> SimpDecl a
hsDeclToSimpDecl decl = case decl of
  Exts.TypeSig l names typeForNames -> SdTypeSig l names typeForNames
  Exts.FunBind l matches -> matchesToFunBind l matches
  Exts.PatBind l pat rhs maybeBinds -> SdPatBind l (hsPatToSimpPat pat) expr
  -- TODO Add a visual representation of data declarations
    where
      expr = whereToLet l rhs maybeBinds
  d -> SdCatchAll d
  
hsBindsToDecls :: Show a => Exts.Binds a -> [SimpDecl a]
hsBindsToDecls binds = case binds of
  Exts.BDecls _ decls -> fmap hsDeclToSimpDecl decls
  _ -> error $ "Unsupported syntax in hsBindsToDecls: " ++ show binds

simpDeclsToHsBinds :: Show a => a -> [SimpDecl a] -> Exts.Binds a
simpDeclsToHsBinds l decls = Exts.BDecls l (fmap simpDeclToHsDecl decls)

simpDeclToHsDecl :: Show a => SimpDecl a -> Exts.Decl a
simpDeclToHsDecl decl = case decl of
  SdPatBind l pat e
    -> Exts.PatBind l (simpPatToHsPat pat) (simpExpToRhs l e) Nothing
  SdTypeSig l names typeForNames -> Exts.TypeSig l names typeForNames
  SdCatchAll d -> d

simpPatToHsPat :: Show a => SimpPat a -> Exts.Pat a
simpPatToHsPat pat = case pat of
  SpVar l n -> Exts.PVar l n
  SpLit l s lit -> Exts.PLit l s lit
  SpApp l n pats -> Exts.PApp l n (fmap simpPatToHsPat pats)
  SpAsPat l n p -> Exts.PAsPat l n (simpPatToHsPat p)
  SpWildCard l -> Exts.PWildCard l

guardedRhsToSelectorAndVal :: Show a => Exts.GuardedRhs a -> SelectorAndVal a
guardedRhsToSelectorAndVal rhs = case rhs of
  Exts.GuardedRhs _ [s] valExp -> SelectorAndVal{svSelector=stmtToExp s
                                                , svVal=hsExpToSimpExp valExp}
  _ -> error $ "Unsupported syntax in guardedRhsToSelectorAndVal: " ++ show rhs
  where
    stmtToExp stmt = case stmt of
      Exts.Qualifier _ e -> hsExpToSimpExp e
      _ -> error
           $ "Unsupported syntax in stmtToExp: " ++ show stmt

selAndValToGuardedRhs :: Show a => a -> SelectorAndVal a -> Exts.GuardedRhs a
selAndValToGuardedRhs l selAndVal = Exts.GuardedRhs
  l
  [Exts.Qualifier l (simpExpToHsExp $ svSelector selAndVal)]
  (simpExpToHsExp $ svVal selAndVal)

hsRhsToExp :: Show a => Exts.Rhs a -> SimpExp a
hsRhsToExp rhs = case rhs of
  Exts.UnGuardedRhs _ e -> hsExpToSimpExp e
  Exts.GuardedRhss l rhss
    -> SeMultiIf l (fmap guardedRhsToSelectorAndVal rhss)

hsAltToSimpAlt :: Show a => Exts.Alt a -> SimpAlt a
hsAltToSimpAlt (Exts.Alt l pat rhs maybeBinds)
  = SimpAlt{saPat=hsPatToSimpPat pat, saVal=whereToLet l rhs maybeBinds}

simpAltToHsAlt :: Show a => a -> SimpAlt a -> Exts.Alt a
simpAltToHsAlt l (SimpAlt pat e)
  = Exts.Alt l (simpPatToHsPat pat) (simpExpToRhs l e) Nothing

ifToGuard :: a -> SimpExp a -> SimpExp a -> SimpExp a -> SimpExp a
ifToGuard l e1 e2 e3
  = SeMultiIf l [SelectorAndVal{svSelector=e1, svVal=e2}
                , SelectorAndVal{svSelector=otherwiseExp, svVal=e3}]
  where
    otherwiseExp = SeName l otherwiseExpStr

pattern FunctionCompositionStr :: String
pattern FunctionCompositionStr = "."

pattern LowPriorityApplicationStr :: String
pattern LowPriorityApplicationStr = "$"

pattern InfixFmapStr :: String
pattern InfixFmapStr = "<$>"

simplifyExp :: SimpExp l -> SimpExp l
simplifyExp e = case e of
  -- Reduce applications of function compositions (e.g. (f . g) x -> f (g x))
  SeApp l2 (SeApp l1 (SeApp _ (SeName _ FunctionCompositionStr) f1) f2) arg
    -> SeApp l1 f1 $ simplifyExp (SeApp l2 f2 arg)
  SeApp l (SeApp _ (SeName _ LowPriorityApplicationStr) exp1) exp2
    -> SeApp l exp1 exp2
  SeApp l1 (SeName l2 InfixFmapStr) arg
    -> SeApp l1 (SeName l2 actionOverParameterizedType) arg
  x -> x

deListifyApp :: Show l => l -> Exts.Exp l -> [Exts.Exp l] -> Exts.Exp l
deListifyApp l = foldl' (Exts.App l)

rewriteTupleSection :: Show l => l -> [Maybe (Exts.Exp l)] -> Exts.Exp l
rewriteTupleSection l mExprs = deListifyApp
                               l
                               (makeVarExp l $ nTupleSectionString expIsJustList)
                               exprs
  where
    exprs = catMaybes mExprs
    expIsJustList = fmap isJust mExprs

-- Rewrite a right section as a lambda.
-- TODO Simplify this type of lambda to use unused ports.
rewriteRightSection :: Show l => l -> Exts.QOp l -> Exts.Exp l -> Exts.Exp l
rewriteRightSection l op expr = Exts.Lambda l [tempPat] appExpr
  where
    tempStr = tempVarPrefix ++ "0"
    tempPat = makePatVar l tempStr
    tempVar = makeVarExp l tempStr
    appExpr = Exts.App l (Exts.App l (qOpToExp op) tempVar) expr

desugarDo :: Show l => [Exts.Stmt l] -> Exts.Exp l
desugarDo stmts = case stmts of
  [Exts.Qualifier _ e] -> e
  (Exts.Qualifier l e : stmtsTail)
    -> Exts.App l (Exts.App l (makeVarExp l thenOperatorStr) e) (desugarDo stmtsTail)
  (Exts.Generator l pat e : stmtsTail)
    -> Exts.App l (Exts.App l (makeVarExp l bindOperatorStr) e)
                  (Exts.Lambda l [pat] (desugarDo stmtsTail))
  (Exts.LetStmt l binds : stmtsTail) -> Exts.Let l binds (desugarDo stmtsTail)
  _ -> error $ "Unsupported syntax in degugarDo: " <> show stmts

desugarEnums :: Show l => l -> String -> [Exts.Exp l] -> SimpExp l
desugarEnums l funcName exprs = hsExpToSimpExp $ deListifyApp l
                                (makeVarExp l funcName)
                                exprs

desugarListComp :: Show l => Exts.Stmt l ->  SimpQStmt l
desugarListComp (Exts.Qualifier l e) = SqQual l (hsExpToSimpExp e)
desugarListComp (Exts.Generator l pat e) = SqGen l (hsExpToSimpExp e) (hsPatToSimpPat pat)
desugarListComp (Exts.LetStmt l binds) = SqLet l (hsBindsToDecls binds)
desugarListComp _ = error "Unsupported syntax in hsQStmtsToSimpStmts'" 
                            
-- http://hackage.haskell.org/package/haskell-src-exts-1.23.0/docs/Language-Haskell-Exts-Syntax.html#g:8
hsExpToSimpExp :: Show a => Exts.Exp a -> SimpExp a
hsExpToSimpExp x = {- --TODO fix simplifyExp $ -} case x of
  Exts.Var l n -> SeName l (qNameToString n)
  Exts.Con l n -> SeName l (qNameToString n)
  Exts.Lit l n -> SeLit l n
  Exts.InfixApp l e1 op e2 ->
    hsExpToSimpExp $ Exts.App l (Exts.App l (qOpToExp op) e1) e2
  Exts.App l f arg -> SeApp l (hsExpToSimpExp f) (hsExpToSimpExp arg)
  Exts.NegApp l e -> hsExpToSimpExp $ Exts.App l (makeVarExp l negateSymbolStr) e
  Exts.Lambda l patterns e
    -> SeLambda l (fmap hsPatToSimpPat patterns) (hsExpToSimpExp e) Nothing
  Exts.Let l bs e -> SeLet l (hsBindsToDecls bs) (hsExpToSimpExp e)
  Exts.If l e1 e2 e3
    -> ifToGuard l (hsExpToSimpExp e1) (hsExpToSimpExp e2) (hsExpToSimpExp e3)
  Exts.Case l e alts -> SeCase l (hsExpToSimpExp e) (fmap hsAltToSimpAlt alts)
  Exts.Paren _ e -> hsExpToSimpExp e
  Exts.List l exprs -> hsExpToSimpExp $ deListifyApp
                       l
                       (makeVarExp l $ nListString $ length exprs)
                       exprs
  Exts.Tuple l _ exprs -> hsExpToSimpExp $ deListifyApp
                       l
                       (makeVarExp l $ nTupleString $ length exprs)
                       exprs
  Exts.TupleSection l _ mExprs -> hsExpToSimpExp $ rewriteTupleSection l mExprs
  Exts.LeftSection l expr op -> hsExpToSimpExp $ Exts.App l (qOpToExp op) expr
  Exts.RightSection l op expr -> hsExpToSimpExp $ rewriteRightSection l op expr
  Exts.Do _ stmts -> hsExpToSimpExp $ desugarDo stmts
  Exts.EnumFrom l e -> desugarEnums l enumFromStr [e]
  Exts.EnumFromTo l e1 e2 -> desugarEnums l enumFromToStr [e1, e2]
  Exts.EnumFromThen l e1 e2 -> desugarEnums l enumFromThenStr [e1, e2]
  Exts.EnumFromThenTo l e1 e2 e3 -> desugarEnums l enumFromThenToStr [e1, e2, e3]
  Exts.MultiIf l rhss -> SeMultiIf l (fmap guardedRhsToSelectorAndVal rhss)
  Exts.ListComp l e1 qStmts -> SeListComp l (hsExpToSimpExp e1) ( desugarListComp <$> filterQStmts qStmts)
  _ -> error $ "Unsupported syntax in hsExpToSimpExp: " ++ show x
  -- BDecls l [Decl l]	
  -- IPBinds l [IPBind l]

  -- (map (hsExpToSimpExp) (mapMaybe hsQStmtsToSimpStmts qStmts))
  -- https://www.haskell.org/onlinereport/exps.html#sect3.11
filterQStmts :: Show l => [Exts.QualStmt l] -> [Exts.Stmt l] 
filterQStmts qStmts = [ x | (Exts.QualStmt _l x) <- qStmts]
-- other are TransformListComp - SQL like syntax in list comp
-- ThenTrans Exp	-- then exp
-- ThenBy Exp Exp	-- then exp by exp
-- GroupBy Exp	-- then group by exp
-- GroupUsing Exp	-- then group using exp
-- GroupByUsing Exp Exp -- then group by exp using exp

simpExpToHsExp :: Show a => SimpExp a -> Exts.Exp a
simpExpToHsExp x = case x of
  -- TODO Sometimes SeName comes from Exts.Con
  --
  -- Put names in parens in case it's an operator
  SeName l str  -> Exts.Paren l (Exts.Var l (strToQName l str))
  -- SeName l str -> (Exts.Var l (strToQName l str))
  SeLit  l lit  -> Exts.Lit l lit
  SeApp l e1 e2 -> Exts.App l (simpExpToHsExp e1) (simpExpToHsExp e2)
  SeLambda l pats e _name ->
    Exts.Lambda l (fmap simpPatToHsPat pats) (simpExpToHsExp e)
  SeLet l decls e -> Exts.Let l (simpDeclsToHsBinds l decls) (simpExpToHsExp e)
  SeCase l e alts ->
    Exts.Case l (simpExpToHsExp e) $ fmap (simpAltToHsAlt l) alts
  SeMultiIf l selsAndVal ->
    Exts.MultiIf l (fmap (selAndValToGuardedRhs l) selsAndVal)
  SeListComp l e1 me2 -> error "TODO simpExpToHsExp SeListComp l e1 me2 " 

-- Parsing

customParseMode :: Exts.ParseMode
customParseMode = Exts.defaultParseMode
  {Exts.extensions =
   [Exts.EnableExtension Exts.MultiParamTypeClasses,
    Exts.EnableExtension Exts.FlexibleContexts,
    Exts.EnableExtension Exts.TupleSections,
    Exts.EnableExtension Exts.MultiWayIf
   ]
  }

customParseDecl :: String -> Exts.Decl Exts.SrcSpanInfo
customParseDecl = Exts.fromParseResult . Exts.parseDeclWithMode customParseMode

stringToSimpDecl :: String -> SimpDecl Exts.SrcSpanInfo
stringToSimpDecl = hsDeclToSimpDecl . customParseDecl

formatString :: String -> Exts.Decl Exts.SrcSpanInfo
formatString = simpDeclToHsDecl . hsDeclToSimpDecl . customParseDecl
