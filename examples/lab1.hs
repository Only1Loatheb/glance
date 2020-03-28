import           Data.Char

middle :: [a] -> a
middle a = a !! (div (length a)  2)

removeDups :: Eq a => [a] -> [a]
removeDups []  = []
removeDups [x] = [x]
removeDups (x : y : xs) | x == y    = x : removeDups xs
                        | otherwise = x : removeDups (y : xs)

adjpairs :: [a] -> [(a, a)]
adjpairs []           = []
adjpairs [_         ] = []
adjpairs (x : y : xs) = (x, y) : adjpairs (y : xs)

removeAllDups :: Eq a => [a] -> [a]
removeAllDups []       = []
removeAllDups (x : xs) = x : removeAllDups (removeThis xs x)

removeThis :: Eq a => [a] -> a -> [a]
removeThis [] _ = []
removeThis (x : xs) toRemove | x == toRemove = removeThis xs toRemove
                             | otherwise     = x : removeThis xs toRemove

string2int :: String -> Int
string2int []         = 0
string2int ('-' : xs) = (-1) * parsePositiveInt xs 0
string2int xs         = parsePositiveInt xs 0

parsePositiveInt :: String -> Int -> Int
parsePositiveInt [x     ] sum = sum * 10 + digitToInt x
parsePositiveInt (x : xs) sum = parsePositiveInt xs (sum * 10 + digitToInt x)

insertAt :: [a] -> a -> Int -> [a]
insertAt []       a _     = [a]
insertAt xs       a 0     = a : xs
insertAt (x : xs) a index = x : insertAt xs a (index - 1)

-- codeCezar :: String -> Int -> String
-- codeCezar [] _ = []
-- codeCezar (x : xs) i
--   | x >= 'A' && x <= 'Z' = (codeCezarLetter x i (ord 'A')) : codeCezar xs i
--   | x >= 'a' && x <= 'z' = (codeCezarLetter x i (ord 'a')) : codeCezar xs i

-- codeCezarLetter x i baseLetterOrd = chr codedletterOrd where
--   codedletterOrd = letterNumberCycled + baseLetterOrd   where
--   letterNumberCycled = (letterNumber + i) `mod` alphabetLength   where
--   letterNumber = (ord x) - baseLetterOrd

-- alphabetLength = (ord 'z') - (ord 'a') + 1

-- decodeCezar :: String -> Int -> String
-- decodeCezar xs i = codeCezar xs (-i)
