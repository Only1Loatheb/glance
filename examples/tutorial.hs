{-# LANGUAGE TemplateHaskell #-} -- guard
{-
This file explains of grapical symbols used in the project.
Symbols corespondes to parts of the haskell syntax.
It is assumed that the reader is familiar with the basics of Haskell.
{--}
-}
{- integer = 1 -}
integer = 1
{- fractional = 1.1 -}
fractional = 1.1
{--}
-- TODO float = (1.1 :: float)
{- character = 'a' -}
character = 'a'
{- string = "a" -}
string = "a"
{- list = [1,2,3,4] where
  [,,,] is list constructor -}
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
{- boolean = True -}
boolean = True
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
{- prefixFunctionApplication = (*) 3 5 -}
prefixFunctionApplication = (*) 3 5
{- infixFunctionApplication = 3 * 5 -}
infixFunctionApplication = 3 * 5
{- ifThenElse = if condition then consequent else alternative -}
ifThenElseExpression = if condition then consequent else alternative 
{- letInExpression = let a = x in a 
    Let expression gets simplified -}
letInExpression = let a = x in a
{- whereDefinitions = f where 
    f x = 3 * x
-}
whereDefinitions = f where 
  f = 3
{- functionComposition = f . g -}
functionComposition = f . g
{- appliedFunctionComposition = (f . g) x-}
appliedFunctionComposition = (f . g) x
{- lambda = \argument -> functionBody argument -}
lambdaExpression = \argument1 argument2 -> functionBodyWith argument1 argument2
{- unpacking.
(unpackedValue1, unpackedValue2, unpackedValue3) = (18.5, 25.0, 30.0) -}
(unpackedValue1, unpackedValue2, unpackedValue3) = (18.5, 25.0, 30.0)
{- unpacking.
(Just unpackedValue) = Just 3 -}
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
Since data constructors are functions, the match icon has a topology similar to
the apply icon.
Now that you are familiar with matches, here's a simple case expression.
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

{-
Compose (bonus section):

The depth of an icon's application tree is called the nesting depth.
For example, The icon representing "factorial (x - 1) * x" above has a nesting
depth of 3, since it is an apply icon (depth=1), inside an apply icon (depth=2),
inside an apply icon (depth=3).

To reduce nesting depth, Glance has an icon that represents an argument applied
to a composition of functions.

For example:
y = f (g x)
which is the same as
y = (f . g) x
-}
y = f (g x)
y = (f . g) x
{-
With a composition of three functions:
y = f (g (h x))
which is the same as
y = (f . g . h) x
-}
y = f (g (h x))
y = (f . g . h) x
{-
Glance figures out automatically when to use the compose icon in order to
reduce the nesting depth.

For example, if we slightly rewrite the factorial function above, Glance
uses a compose icon for the else expression.

To enable the compose icon, we change the expression
factorial (x - 1) * x
to
x * factorial (x - 1)

Glance essentially rewrites the second expression as:
(x *) . factorial . (x -) $ 1

Notice that the nesting depth has been reduced from 3 to 2.
-}
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
listComprehension = [fApplied item | item <- generator
                                      , gcd item localDefinition == 1
                                      , let localDefinition = item + 1]
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