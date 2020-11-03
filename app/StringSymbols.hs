module StringSymbols
  (
  ifConditionConst
  , otherwiseExpStr
  , negateSymbolStr
  , enumComaStr
  , enumDotsStr
  , thenOperatorStr
  , bindOperatorStr
  , typeSignatureSeparatorStr
  , typeNameSeparatorStr
  , negativeLiteralStr
  , patternWildCardStr
  , unusedArgumentStr
  , defaultPatternNameStr
  , nTupleString
  , nTupleSectionString
  , nListString
  , actionOverParameterizedType
  , patternSubscribedValueStr
  , fractionalSeparatorStr
  , showLiteral
  , nameToString
  , qNameToString
  , showSignlessLit
  , isTempLabel
  , getTempVarLabel
  , getFuncDefLabel
  , sourceCodeDiagramLabel
  , enumLBracketStr
  , enumRBracketStr
  ) where

import qualified Language.Haskell.Exts as Exts
import Data.List(isPrefixOf)


fractionalSeparatorStr :: String
fractionalSeparatorStr = "%"

ifConditionConst :: String
ifConditionConst = "True"

-- tempvar
tempPrefix :: String
tempPrefix = " "

tempVarPrefix :: String
tempVarPrefix = tempPrefix ++ "tempvar"

getTempVarLabel :: Show a => a -> String
getTempVarLabel x = tempVarPrefix ++ show x

lambdaSymbolStr :: String
lambdaSymbolStr = tempPrefix ++ "lambda"

getFuncDefLabel :: Show a => a -> Maybe String -> String
getFuncDefLabel _ (Just str) = str
getFuncDefLabel lambdaName _ = lambdaSymbolStr ++ show lambdaName

isTempLabel :: String -> Bool
isTempLabel str = tempPrefix `isPrefixOf` str
-- tempvar

qualifiedSeparatorStr :: String
qualifiedSeparatorStr = "."

unitConstructorStr :: String
unitConstructorStr = "()"

listTypeConstructorStr :: String
listTypeConstructorStr = "[]"

functionConstructor :: String
functionConstructor = "(->)"

listDataConstructorStr :: String
listDataConstructorStr = "(:)"

unboxedTupleConstructorStr :: String
unboxedTupleConstructorStr = "(# #)"
-- n-ary tuple type and data constructors (,) etc, possibly boxed (#,#)

otherwiseExpStr :: String
otherwiseExpStr = "otherwise"

-- isLambdaName 

negateSymbolStr :: String
negateSymbolStr = "negate"

enumComaStr :: String
enumComaStr = ","

enumDotsStr :: String
enumDotsStr = ".."

enumLBracketStr :: String
enumLBracketStr = "["

enumRBracketStr :: String
enumRBracketStr = "]"

thenOperatorStr :: String
thenOperatorStr = ">>"

bindOperatorStr :: String
bindOperatorStr = ">>="

typeSignatureSeparatorStr :: String
typeSignatureSeparatorStr = " :: "

typeNameSeparatorStr :: String
typeNameSeparatorStr = ","


negativeLiteralStr :: String
negativeLiteralStr = "-"

patternWildCardStr :: String
patternWildCardStr = "_"

unusedArgumentStr :: String
unusedArgumentStr = "unused_argument"

defaultPatternNameStr :: String
defaultPatternNameStr = ""

nTupleString :: Int -> String
nTupleString n = '(' : replicate (n -1) ',' ++ ")"

-- TODO Unit tests for this
nTupleSectionString :: [Bool] -> String
nTupleSectionString bools = '(' : (commas ++ ")") where
  commas = case concatMap trueToUnderscore bools of
    [] -> []
    (_:xs) -> xs

  trueToUnderscore x = if x
    then ",_"
    else ","

nListString :: Int -> String
nListString 1 = "[•]"
nListString n = '[' : replicate (n -1) ',' ++ "]"

actionOverParameterizedType :: String
actionOverParameterizedType = "fmap"

patternSubscribedValueStr :: String
patternSubscribedValueStr = "@"

sourceCodeDiagramLabel :: String
sourceCodeDiagramLabel = "Source code:\n"

nameToString :: Exts.Name l -> String
nameToString (Exts.Ident _ s) = s
nameToString (Exts.Symbol _ s) = s

qNameToString :: Show l => Exts.QName l -> String
qNameToString qName = case qName of
  Exts.Qual _ (Exts.ModuleName _ modName) name
    -> modName ++ qualifiedSeparatorStr ++ nameToString name
  Exts.UnQual _ name -> nameToString name
  Exts.Special _ constructor -> case constructor of
    Exts.UnitCon _ -> unitConstructorStr
    Exts.ListCon _ -> listTypeConstructorStr
    Exts.FunCon _ -> functionConstructor
    Exts.TupleCon _ _ n -> nTupleString n
    Exts.Cons _ -> listDataConstructorStr
    -- unboxed singleton tuple constructor
    Exts.UnboxedSingleCon _ -> unboxedTupleConstructorStr
    -- Exts.ExprHole _ -> "_" -- TODO find out why it is not there
    _ -> error $ "Unsupported syntax in qNameToSrting: " <> show qName

showFracLiteral ::  Rational -> String
showFracLiteral = concat . words . show

-- TODO: Test the unboxed literals
-- TODO: Print the Rational as a floating point.

showLiteral :: Exts.Sign l -> Exts.Literal l -> String
showLiteral (Exts.Signless _) lit = showSignlessLit lit
showLiteral (Exts.Negative _) lit = negativeLiteralStr ++ showSignlessLit lit

showSignlessLit :: Exts.Literal l -> String
showSignlessLit (Exts.Int _ x _) = show x
showSignlessLit (Exts.Char _ x _) = show x
showSignlessLit (Exts.String _ x _) = show x
showSignlessLit (Exts.Frac _ x _) = showFracLiteral x
showSignlessLit (Exts.PrimInt _ x _) = show x
showSignlessLit (Exts.PrimWord _ x _) = show x
showSignlessLit (Exts.PrimFloat _ x _) = show x
showSignlessLit (Exts.PrimDouble _ x _) = show x
showSignlessLit (Exts.PrimChar _ x _) = show x
showSignlessLit (Exts.PrimString _ x _) = show x