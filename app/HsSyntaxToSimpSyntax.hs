{-# LANGUAGE PatternSynonyms #-}
module HsSyntaxToSimpSyntax (
  SimpExp(..)
  , SrcRef(..)
  , SelectorAndVal(..)
  , SimpAlt(..)
  , SimpDecl(..)
  , SimpPat(..)
  , SimpQStmt(..)
  , SimpQStmtCore(..)
  , SimpExpCore(..)
  , SimpPatCore(..)
  , SimpDeclCore(..)
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

import Types (
  SrcRef
  )


import qualified Language.Haskell.Exts as Exts

import StringSymbols(
  qualifiedSeparatorStr
  , unitConstructorStr
  , listTypeConstructorStr
  , functionConstructor
  , listDataConstructorStr
  , unboxedTupleConstructorStr
  , getTempVarLabel
  , otherwiseExpStr
  , negateSymbolStr
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
-- class WithReference aa where
--   -- | Retrieve the annotation of an AST node.
--   ref :: aa -> SrcRef

-- A simplified Haskell expression.
data SimpExp = SimpExp SrcRef SimpExpCore
  deriving (Show, Eq)


data SimpExpCore =
  SeName 
    { nameStr :: String }
  | SeLit
    { literal :: Exts.Literal Exts.SrcSpanInfo}
  | SeApp
    { function :: SimpExp
    , argument :: SimpExp}
  | SeLambda
    { patAplications :: [SimpPat]
    , bodyExpresion :: SimpExp
    , laNameStr :: (Maybe String)}
  | SeLet
    { declarations :: [SimpDecl]
    , bodyExpresion :: SimpExp}
  | SeCase
    { bodyExpresion :: SimpExp
    , alternatives :: [SimpAlt]}
  | SeMultiIf
    { selectorAndVal :: [SelectorAndVal] }
  | SeListComp
    { bodyExpresion :: SimpExp
    , qualifyingExpresion ::[SimpQStmt]} 
  | SeListGen
    { from :: SimpExp
    , maybeThen :: Maybe SimpExp
    , maybeTo :: Maybe SimpExp} 
  deriving (Show, Eq)

data SimpStmt = SimpStmt SrcRef String deriving (Show, Eq)

data SelectorAndVal = SelectorAndVal
  { selectorAndValRef :: SrcRef
  , svSelector :: SimpExp
  , svVal :: SimpExp
  }
  deriving (Show, Eq)

data SimpAlt = SimpAlt
  { simpAltRef :: SrcRef
  , saPat :: SimpPat
  , saVal :: SimpExp
  }
  deriving (Show, Eq)


data SimpDecl = SimpDecl {
  simpDeclRef :: SrcRef
  , declCore :: SimpDeclCore
  }
  deriving (Show, Eq)

  
data SimpDeclCore = 
  -- These don't have decl lists, since only lets have decl lists
  SdPatBind
    { patternBind :: SimpPat
    , bindedExp :: SimpExp}
  | SdTypeSig 
    { typeSigNames :: [Exts.Name Exts.SrcSpanInfo]
    , typeBody :: Exts.Type Exts.SrcSpanInfo}
  -- TODO Add a visual representation of data declarations
  | SdCatchAll 
    { declaration :: Exts.Decl Exts.SrcSpanInfo}
  deriving (Show, Eq)

data SimpPat = SimpPat {
  simpPatref :: SrcRef
  , patCore :: SimpPatCore
  }
  deriving (Show, Eq)

data SimpPatCore =   
  SpVar 
    { varName :: Exts.Name Exts.SrcSpanInfo}
  | SpLit 
    { litSign :: Exts.Sign Exts.SrcSpanInfo
    , absValue :: Exts.Literal Exts.SrcSpanInfo}
  | SpApp 
    { appliedCons :: Exts.QName Exts.SrcSpanInfo
    , arguments :: [SimpPat]}
  | SpAsPat 
    { alias :: Exts.Name Exts.SrcSpanInfo
    , aliasedPattern :: SimpPat}
  | SpWildCard
  deriving (Show, Eq)

data SimpQStmt = SimpQStmt SrcRef SimpQStmtCore 
  deriving (Show, Eq)

data SimpQStmtCore = 
  SqQual
    { qualifier :: SimpExp}
  | SqGen
    { valueGenerator :: SimpExp
    , itemPattern :: SimpPat}
  | SqLet 
    { declarationBinds :: [SimpDecl]}
  deriving (Show, Eq)

-- Helper functions
srcRef :: Exts.SrcSpanInfo -> SrcRef
srcRef = Exts.srcInfoSpan 

srcSpanInfo :: SrcRef -> Exts.SrcSpanInfo
srcSpanInfo span = Exts.infoSpan span []

nameToQname :: Exts.SrcSpanInfo -> Exts.Name Exts.SrcSpanInfo -> Exts.QName Exts.SrcSpanInfo
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

simpPatNameStr :: SimpPatCore -> String
simpPatNameStr simpPatCore = case simpPatCore of
  SpVar n        -> nameToString n
  SpLit sign lit -> showLiteral sign lit
  SpApp qn _     -> qNameToString qn
  SpAsPat n _    -> nameToString n
  _              -> error "unsupported pattern"

simpExpToRhs :: Exts.SrcSpanInfo -> SimpExp -> Exts.Rhs Exts.SrcSpanInfo
simpExpToRhs l e = Exts.UnGuardedRhs l (simpExpToHsExp e)
    
--

hsPatToSimpPat :: Exts.Pat Exts.SrcSpanInfo -> SimpPat
hsPatToSimpPat p = SimpPat (srcRef $ Exts.ann p) simpPatCore where
  simpPatCore = case p of
    Exts.PVar l n -> SpVar n
    Exts.PLit l sign lit -> SpLit sign lit
    Exts.PInfixApp l p1 qName p2 -> patCore $ hsPatToSimpPat (Exts.PApp l qName [p1, p2])
    Exts.PApp l name patts -> SpApp name (fmap hsPatToSimpPat patts)
    Exts.PTuple l _ patts -> SpApp
                            ((strToQName l . nTupleString . length) patts)
                            (fmap hsPatToSimpPat patts)
    Exts.PParen l pat -> patCore $ hsPatToSimpPat pat
    Exts.PAsPat l name pat -> SpAsPat name (hsPatToSimpPat pat)
    Exts.PWildCard l -> SpWildCard
    Exts.PList l patts -> SpApp
                          ((strToQName l . nListString . length) patts)
                          (fmap hsPatToSimpPat patts)
    _ -> error $  "Unsupported syntax in hsPatToSimpPat: " <> show p

-- BEGIN BEGIN BEGIN BEGIN BEGIN matchesToFunBind
whereToLet :: Exts.SrcSpanInfo -> Exts.Rhs Exts.SrcSpanInfo  -> Maybe (Exts.Binds Exts.SrcSpanInfo ) -> SimpExp
whereToLet l rhs maybeBinds = val
  where
    rhsExp = hsRhsToExp rhs
    val = case maybeBinds of
      Nothing -> rhsExp
      Just binds -> SimpExp s $ SeLet (hsBindsToDecls binds) rhsExp where
        s = srcRef l

matchToDeclWLambda :: Exts.Match Exts.SrcSpanInfo -> SimpDecl
matchToDeclWLambda (Exts.Match l name patterns rhs maybeWhereBinds)
  = SimpDecl s $ SdPatBind (SimpPat s $ SpVar name) lambda where
    s = srcRef l
    lambda = SimpExp s $ SeLambda
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
  
matchesToFunBind :: Exts.SrcSpanInfo -> [Exts.Match Exts.SrcSpanInfo] -> Exts.Match Exts.SrcSpanInfo
matchesToFunBind l [] = error $ "Empty matches in matchesToFunBind."
matchesToFunBind l [match] = match 
matchesToFunBind l allMatches@(firstMatch : _) = multipleMatchesToCase l firstMatch allMatches

-- TODO combine srcLoc
multipleMatchesToCase :: Exts.SrcSpanInfo -> Exts.Match Exts.SrcSpanInfo -> [Exts.Match Exts.SrcSpanInfo] -> Exts.Match Exts.SrcSpanInfo
multipleMatchesToCase srcLoc (Exts.Match _ funName pats _ _ ) allMatches
  = Exts.Match srcLoc funName casePats rhs Nothing where
    -- There is a special case in Icons.hs/makeLabelledPort to exclude " casevar"
    caseStrings = fmap getTempVarLabel [0..(length pats - 1)]
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

hsDeclToSimpDecl :: Exts.Decl Exts.SrcSpanInfo -> SimpDecl
hsDeclToSimpDecl decl = SimpDecl (srcRef $ Exts.ann decl)  simpDeclCore where
  simpDeclCore = case decl of
    Exts.TypeSig l names typeForNames -> SdTypeSig names typeForNames
    Exts.FunBind l matches -> declCore $ matchToDeclWLambda $ matchesToFunBind l matches
    Exts.PatBind l pat rhs maybeBinds -> SdPatBind (hsPatToSimpPat pat) expr
    -- TODO Add a visual representation of data declarations
      where expr = whereToLet l rhs maybeBinds
    d -> SdCatchAll d
  
hsBindsToDecls :: Exts.Binds Exts.SrcSpanInfo -> [SimpDecl]
hsBindsToDecls binds = case binds of
  Exts.BDecls _ decls -> fmap hsDeclToSimpDecl decls
  _ -> error $ "Unsupported syntax in hsBindsToDecls: " ++ show binds

simpDeclsToHsBinds :: SrcRef -> [SimpDecl] -> Exts.Binds Exts.SrcSpanInfo
simpDeclsToHsBinds s decls = Exts.BDecls l (fmap simpDeclToHsDecl decls)
  where  l = srcSpanInfo s

simpDeclToHsDecl :: SimpDecl -> Exts.Decl Exts.SrcSpanInfo
simpDeclToHsDecl (SimpDecl s decl) = hsDecl where
  hsDecl = case decl of
    SdPatBind pat e -> Exts.PatBind l (simpPatToHsPat pat) (simpExpToRhs l e) Nothing
    SdTypeSig names typeForNames -> Exts.TypeSig l names typeForNames
    SdCatchAll d -> d
  l = srcSpanInfo s
  

simpPatToHsPat :: SimpPat -> Exts.Pat Exts.SrcSpanInfo
simpPatToHsPat (SimpPat s pat) = hsPat where
  hsPat = case pat of
    SpVar n -> Exts.PVar l n
    SpLit s lit -> Exts.PLit l s lit
    SpApp n pats -> Exts.PApp l n (fmap simpPatToHsPat pats)
    SpAsPat n p -> Exts.PAsPat l n (simpPatToHsPat p)
    SpWildCard -> Exts.PWildCard l
  l = srcSpanInfo s

guardedRhsToSelectorAndVal :: Exts.GuardedRhs Exts.SrcSpanInfo -> SelectorAndVal
guardedRhsToSelectorAndVal rhs = case rhs of
  Exts.GuardedRhs l [s] valExp -> SelectorAndVal (srcRef l) (stmtToExp s) (hsExpToSimpExp valExp)
  _ -> error $ "Unsupported syntax in guardedRhsToSelectorAndVal: " ++ show rhs
  where
    stmtToExp stmt = case stmt of
      Exts.Qualifier _ e -> hsExpToSimpExp e
      _ -> error
           $ "Unsupported syntax in stmtToExp: " ++ show stmt

selAndValToGuardedRhs :: SrcRef -> SelectorAndVal -> Exts.GuardedRhs  Exts.SrcSpanInfo
selAndValToGuardedRhs s selAndVal = Exts.GuardedRhs
  l
  [Exts.Qualifier l (simpExpToHsExp $ svSelector selAndVal)]
  (simpExpToHsExp $ svVal selAndVal)
    where
      l = srcSpanInfo s


hsRhsToExp :: Exts.Rhs Exts.SrcSpanInfo -> SimpExp
hsRhsToExp rhs = case rhs of
  Exts.UnGuardedRhs _ e -> hsExpToSimpExp e
  Exts.GuardedRhss l rhss
    -> SimpExp s $ SeMultiIf (fmap guardedRhsToSelectorAndVal rhss) where
      s = srcRef l

hsAltToSimpAlt :: Exts.Alt Exts.SrcSpanInfo -> SimpAlt
hsAltToSimpAlt (Exts.Alt l pat rhs maybeBinds)
  = SimpAlt (srcRef l) (hsPatToSimpPat pat) (whereToLet l rhs maybeBinds)

simpAltToHsAlt :: SrcRef -> SimpAlt -> Exts.Alt Exts.SrcSpanInfo
simpAltToHsAlt s (SimpAlt ss pat e) = Exts.Alt l (simpPatToHsPat pat) (simpExpToRhs ll e) Nothing
  where
    l = srcSpanInfo s
    ll = srcSpanInfo ss

ifToGuard :: SrcRef -> SimpExp -> SimpExp -> SimpExp -> SimpExp
ifToGuard l e1 e2 e3 = SimpExp l 
  $ SeMultiIf [ SelectorAndVal l e1 e2
              , SelectorAndVal l otherwiseExp e3
              ]
  where
    otherwiseExp = SimpExp l $ SeName otherwiseExpStr

pattern FunctionCompositionStr :: String
pattern FunctionCompositionStr = "."

pattern LowPriorityApplicationStr :: String
pattern LowPriorityApplicationStr = "$"

pattern InfixFmapStr :: String
pattern InfixFmapStr = "<$>"

simplifyExp :: SimpExp -> SimpExp
simplifyExp e = case e of
  -- Reduce applications of function compositions (e.g. (f . g) x -> f (g x))
  SimpExp l1 (SeApp 
    (SimpExp l2 (SeApp 
      (SimpExp _  (SeApp 
        (SimpExp _  (SeName FunctionCompositionStr))
        f1))
      f2))
    arg)
    -> SimpExp l1 (SeApp
        f1 
        (simplifyExp (SimpExp l2 (SeApp f2 arg)))
      )
  SimpExp l (SeApp
    (SimpExp _ (SeApp
      (SimpExp _ (SeName LowPriorityApplicationStr))
      exp1))
    exp2)
    -> SimpExp l (SeApp exp1 exp2)
  SimpExp l1 (SeApp
    (SimpExp l2 (SeName InfixFmapStr))
    arg)
    -> SimpExp l1 (SeApp 
        (SimpExp l2 (SeName actionOverParameterizedType)) 
      arg)
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
    tempStr = getTempVarLabel "0"
    tempPat = makePatVar l tempStr
    tempVar = makeVarExp l tempStr
    appExpr = Exts.App l (Exts.App l (qOpToExp op) tempVar) expr

desugarDo :: Show l => [Exts.Stmt l] -> Exts.Exp l
desugarDo stmts = case stmts of
  [Exts.Qualifier _ e] -> e
  (Exts.Qualifier l e : stmtsTail)      -> Exts.App l 
    (Exts.App l (makeVarExp l thenOperatorStr) e) 
    (desugarDo stmtsTail)
  (Exts.Generator l pat e : stmtsTail)  -> Exts.App l 
    (Exts.App l (makeVarExp l bindOperatorStr) e)
    (Exts.Lambda l [pat] (desugarDo stmtsTail))
  (Exts.LetStmt l binds : stmtsTail)    -> Exts.Let l binds (desugarDo stmtsTail)
  _ -> error $ "Unsupported syntax in degugarDo: " <> show stmts

desugarEnums :: Exts.SrcSpanInfo 
  -> Exts.Exp Exts.SrcSpanInfo 
  -> Maybe (Exts.Exp Exts.SrcSpanInfo) 
  -> Maybe (Exts.Exp Exts.SrcSpanInfo) 
  -> SimpExp
desugarEnums l eFrom eThen eTo = SimpExp (srcRef l) $ SeListGen
  (hsExpToSimpExp eFrom)
  (fmap hsExpToSimpExp eThen)
  (fmap hsExpToSimpExp eTo)

desugarListComp :: Exts.Stmt Exts.SrcSpanInfo ->  SimpQStmt
desugarListComp qStmt = SimpQStmt (srcRef $ Exts.ann qStmt) simpQStmtCore where
  simpQStmtCore = case qStmt of
    (Exts.Qualifier l e)     -> SqQual (hsExpToSimpExp e)
    (Exts.Generator l pat e) -> SqGen (hsExpToSimpExp e) (hsPatToSimpPat pat)
    (Exts.LetStmt l binds)   -> SqLet (hsBindsToDecls binds)
    _                        -> error "Unsupported syntax in hsQStmtsToSimpStmts'" 
                            
-- http://hackage.haskell.org/package/haskell-src-exts-1.23.0/docs/Language-Haskell-Exts-Syntax.html#g:8
hsExpToSimpExp :: Exts.Exp Exts.SrcSpanInfo -> SimpExp
hsExpToSimpExp x = simplifyExp $ case x of
  Exts.Var l n -> SimpExp (srcRef l) $ SeName (qNameToString n)
  Exts.Con l n -> SimpExp (srcRef l) $ SeName (qNameToString n)
  Exts.Lit l n -> SimpExp (srcRef l) $ SeLit n
  Exts.InfixApp l e1 op e2 ->
    hsExpToSimpExp $ Exts.App l (Exts.App l (qOpToExp op) e1) e2
  Exts.App l f arg -> SimpExp (srcRef l) $ SeApp (hsExpToSimpExp f) (hsExpToSimpExp arg)
  Exts.NegApp l e -> hsExpToSimpExp $ Exts.App l (makeVarExp l negateSymbolStr) e
  Exts.Lambda l patterns e -> SimpExp (srcRef l) $ SeLambda (fmap hsPatToSimpPat patterns) (hsExpToSimpExp e) Nothing
  Exts.Let l bs e ->  SimpExp (srcRef l) $ SeLet (hsBindsToDecls bs) (hsExpToSimpExp e)
  Exts.If l e1 e2 e3 -> ifToGuard (srcRef l) (hsExpToSimpExp e1) (hsExpToSimpExp e2) (hsExpToSimpExp e3)
  Exts.Case l e alts -> SimpExp (srcRef l) $ SeCase (hsExpToSimpExp e) (fmap hsAltToSimpAlt alts)
  Exts.Paren l e -> hsExpToSimpExp e
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
  Exts.EnumFrom l e -> desugarEnums l e Nothing Nothing
  Exts.EnumFromTo l e1 e2 -> desugarEnums l e1 Nothing (Just e2)
  Exts.EnumFromThen l e1 e2 -> desugarEnums l e1 (Just e2) Nothing
  Exts.EnumFromThenTo l e1 e2 e3 -> desugarEnums l e1 (Just e2) (Just e3)
  Exts.MultiIf l rhss -> SimpExp (srcRef l) $ SeMultiIf (fmap guardedRhsToSelectorAndVal rhss)
  Exts.ListComp l e1 qStmts -> SimpExp (srcRef l) $ SeListComp (hsExpToSimpExp e1) (desugarListComp <$> filterQStmts qStmts)
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

simpExpToHsExp :: SimpExp -> Exts.Exp Exts.SrcSpanInfo
simpExpToHsExp (SimpExp s x) = case x of
  -- TODO Sometimes SeName comes from Exts.Con
  --
  -- Put names in parens in case it's an operator
  SeName str  -> Exts.Paren l (Exts.Var l (strToQName l str))
  -- SeName l str -> (Exts.Var l (strToQName l str))
  SeLit lit  -> Exts.Lit l lit
  SeApp e1 e2 -> Exts.App l (simpExpToHsExp e1) (simpExpToHsExp e2)
  SeLambda pats e _name ->
    Exts.Lambda l (fmap simpPatToHsPat pats) (simpExpToHsExp e)
  SeLet decls e -> Exts.Let l (simpDeclsToHsBinds s decls) (simpExpToHsExp e)
  SeCase e alts ->
    Exts.Case l (simpExpToHsExp e) $ fmap (simpAltToHsAlt s) alts
  SeMultiIf selsAndVal ->
    Exts.MultiIf l (fmap (selAndValToGuardedRhs s) selsAndVal)
  SeListComp e1 me2 -> error "TODO simpExpToHsExp SeListComp l e1 me2 "
  where 
    l = srcSpanInfo s

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

stringToSimpDecl :: String -> SimpDecl
stringToSimpDecl = hsDeclToSimpDecl . customParseDecl

formatString :: String -> Exts.Decl Exts.SrcSpanInfo
formatString = simpDeclToHsDecl . hsDeclToSimpDecl . customParseDecl
