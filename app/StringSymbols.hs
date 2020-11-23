module StringSymbols
  (
  ifConditionConst
  , otherwiseExpStr
  , negateSymbolStr
  , listDotsStr
  , thenOperatorStr
  , bindOperatorStr
  , typeSignatureSeparatorStr
  , typeNameSeparatorStr
  , negativeLiteralStr
  , patternWildCardStr
  , unusedArgumentStr
  , defaultPatternNameStr
  , nTuplePatternString
  , nTupleSectionDelimiters
  , nListDelimiters
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
  , enumFromDelimiters
  , enumFromToDelimiters
  , enumFromThenDelimiters
  , enumFromThenToDelimiters
  , nListPatternString
  , nTupleDelimiters
  ) where

import qualified Language.Haskell.Exts as Exts
import Data.List(groupBy, isPrefixOf)
import Types(Delimiters)
import Data.Maybe (isJust)


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

listDotsStr :: String
listDotsStr = ".."

enumFromDelimiters :: Delimiters
enumFromDelimiters = ["[","..]"]

enumFromToDelimiters :: Delimiters
enumFromToDelimiters = ["[","..","]"]

enumFromThenDelimiters :: Delimiters
enumFromThenDelimiters = ["[",",","..]"]

enumFromThenToDelimiters :: Delimiters
enumFromThenToDelimiters = ["[",",","..","]"]


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

nTuplePatternString :: Int -> String
nTuplePatternString n = concat $ nTupleDelimiters n

nTupleDelimiters :: Int -> Delimiters
nTupleDelimiters n =  "(" : replicate (n -1) "," ++ [")"]

-- TODO Unit tests for this
nTupleSectionDelimiters :: Eq a => [Maybe a] -> Delimiters
nTupleSectionDelimiters maybeExp = delims where
  groupsOfMaybe = groupBy isSameMaybe maybeExp
  countsOfMaybe = map lengthOfGroup groupsOfMaybe
  delims = getDelims countsOfMaybe

  isSameMaybe m1 m2 = isJust m1 == isJust m2

  lengthOfGroup :: [Maybe a] -> Either Int Int
  lengthOfGroup xs = case head xs of
    Just _ -> Left $ length xs
    Nothing -> Right $ length xs

  getDelims [] = ["()"]
  getDelims [Left exprs] = "(" : expDelims exprs ++ [")"]
  getDelims [Right exprs] = [concat ("(•" : replicate (exprs - 1) ",•") ++ ")"]
  getDelims (x:xs) = headDelims x ++ concatMap middleDelims (init xs) ++ lastDelims (last xs)

  headDelims (Left exprs) = "(" : expDelims exprs
  headDelims (Right n) = [concat ("(" : replicate n "•,")]

  middleDelims (Left exprs) = expDelims exprs
  middleDelims (Right n) = [concat ("," :replicate n "•,")]
  
  lastDelims (Left exprs) = expDelims exprs ++ [")"]
  lastDelims (Right n) = [concat (replicate n ",•"), ")"]

  expDelims exprs = replicate (exprs - 1) ","

nListDelimiters :: Int -> Delimiters
nListDelimiters n = "[" : replicate (n -1) "," ++ ["]"]

nListPatternString :: Int -> String
nListPatternString 1 = "[•]"
nListPatternString n = concat $ nListDelimiters n


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
    Exts.TupleCon _ _ n -> nTuplePatternString n
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