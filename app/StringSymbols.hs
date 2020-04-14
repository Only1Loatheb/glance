module StringSymbols
  (
  ifConditionConst
  , tempVarPrefix
  , qualifiedSeparatorStr
  , unitConstructorStr
  , listTypeConstructorStr
  , functionConstructor
  , listDataConstructorStr
  , unboxedTupleConstructorStr
  , otherwiseExpStr
  , lambdaSymbolStr
  , negateSymbolStr
  , enumFromStr
  , enumFromToStr
  , enumFromThenStr
  , enumFromThenToStr
  , thenOperatorStr
  , bindOperatorStr
  , listCompositionPlaceholderStr
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
  ) where

ifConditionConst :: String
ifConditionConst = "True"

tempVarPrefix :: String
tempVarPrefix = " tempvar"

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

lambdaSymbolStr :: String
lambdaSymbolStr = "lambda"

negateSymbolStr :: String
negateSymbolStr = "negate"

enumFromStr :: String
enumFromStr = "enumFrom"

enumFromToStr :: String
enumFromToStr = "enumFromTo"

enumFromThenStr :: String
enumFromThenStr = "enumFromThen"

enumFromThenToStr :: String
enumFromThenToStr = "enumFromThenTo"

thenOperatorStr :: String
thenOperatorStr = ">>"

bindOperatorStr :: String
bindOperatorStr = ">>="

listCompositionPlaceholderStr = "listComp"

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
