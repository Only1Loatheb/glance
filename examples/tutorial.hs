{-# LANGUAGE TemplateHaskell #-}
{-
This file explains of grapical symbols used in the project.
Symbols corespondes to parts of the haskell syntax.
It is assumed that the reader is familiar with the basics of Haskell.
-}
{- integer = 1 -}
integer = 1
{- fractional = 1.1 -}
fractional = 1.1
{- character = 'a' -}
character = 'a'
{- string = "a" -}
string = "a"
{- boolean = True -}
boolean = True
{- list = [1,2,3,4] where
  [,,,] describes list -}
list = [1,2,3,4]
{- listWithOneElement = [1] -}
listWithOneElement = [1]
{- emptyList = [] -}
emptyList = []
{- tuple = (1,2,3) where
  (,,) is tuple constructor-}
tuple = (1,2,3)
{- emptyTuple = () -}
emptyTuple = ()
{- enumFrom = [1..] -}
enumFrom = [1..]
{- enumFromTo = [1..5] -}
enumFromTo = [1..5]
{- enumFromThen = [1,2..] -}
enumFromThen = [1,2..]
{- enumFromThenTo = [1,2..10] -}
enumFromThenTo = [1,2..10]
{- result = function argument -}
result = function argument
{- prefixOperatorApplication = (*) 3 5 -}
prefixOperatorApplication = (*) 3 5
{- infixOperatorApplication = 3 * 5 -}
infixOperatorApplication = 3 * 5
{- ifThenElse = if condition then consequent else alternative -}
ifThenElseExpression = if condition then consequent else alternative 
{- letInExpression = let a = x in a 
    Let expression gets simplified -}
letInExpression = let a = x in a
{- whereDefinitions = f where 
  f = 3
-}
whereDefinitions = f where 
  f = 3
{- functionComposition = f . g -}
functionComposition = f . g
{- appliedFunctionComposition = (f . g) x-}
appliedFunctionComposition = (f . g) x
{- lambdaExpression = \argument1 argument2 -> functionBodyWith argument1 argument2 -}
lambdaExpression = \argument1 argument2 -> functionBodyWith argument1 argument2
{- (unpackedValue1, unpackedValue2, unpackedValue3) = (18.5, 25.0, 30.0) -}
(unpackedValue1, unpackedValue2, unpackedValue3) = (18.5, 25.0, 30.0)
{- (Just unpackedValue) = Just 3 -}
(Just unpackedValue) = Just 3
{- ignoreValueInPattern _ = result -}
ignoreValueInPattern _ = result
{- paterrn function definition 
patternFunctionDefinition = functionName where 
  functionName argument1Pattern1 argument2Pattern1 = functionValue1 argument1Pattern1 argument2Pattern1
  functionName argument1Pattern2 argument2Pattern2 = functionValue2 argument1Pattern2 argument2Pattern2
-}
patternFunctionDefinition = functionName where 
  functionName argument1Pattern1 argument2Pattern1 = functionValue1 argument1Pattern1 argument2Pattern1
  functionName argument1Pattern2 argument2Pattern2 = functionValue2 argument1Pattern2 argument2Pattern2
{-
Here's a simple case expression.
caseExpression = case expression of 
  Right expressionPattern1 -> result1  
  Left expressionPattern2 -> result2 
-}
caseExpression = case expression of 
  Right expressionPattern1 -> result1  
  Left expressionPattern2 -> result2 

{- Guards:
guards argument1 argument2
  | condition1 argument1 argument2 = value1
  | condition2 argument1 argument2 = value2
  | otherwise = value3
-}
guards argument1 argument2
  | condition1 argument1 argument2 = value1
  | condition2 argument1 argument2 = value2
  | otherwise = value3    
{-
Recursion.
factorial x =
  if x == 0
    then 1
    else factorial (x - 1) * x
-}
factorial x =
  if x == 0
    then 1
    else factorial (x - 1) * x

{- Composition:
y = f (g x)
-}
y = f (g x)
{- which is the same as
y = (f . g) x
-}
y = (f . g) x
{- doNotation = do 
    item <- generator
    let localDefinition = item + 1
    guard (gcd localDefinition item == 1)
    return (fApplied item)
-}
doNotation = do 
  generatedItem <- generator
  let localDefinition = generatedItem + 1
  guard (gcd localDefinition generatedItem == 1)
  return (fApplied generatedItem)
{-
listComprehension = [fApplied generatedItem | generatedItem <- generator
                                      , gcd generatedItem localDefinition == 1
                                      , let localDefinition = generatedItem + 1]
-}
listComprehension = [fApplied generatedItem | generatedItem <- generator
                                      , gcd generatedItem localDefinition == 1
                                      , let localDefinition = generatedItem + 1]
{- nestedFunctionApplication = (*) ((+) 8 7) 2 -}
nestedFunctionApplication = (*) ((+) 8 7) 2
{- nestedLambda = (\x1 -> (\x2 -> (\x3 -> sum [x1, x2, x3]))) -}
nestedLambda = (\x1 -> (\x2 -> (\x3 -> sum [x1, x2, x3])))
{-As soon as an expression is used more than once, the tree topology is lost,
and Glance extracts the sub-expression into a separate (non-nested) icon.
y = foo (3 + bazOf2) (8 * bazOf2) where bazOf2 = baz 2
-}
y = foo (3 + bazOf2) (8 * bazOf2) where bazOf2 = baz 2