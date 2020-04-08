-- import Data.Ord
-- import Control.Monad

single :: [a] -> [[a]]
single [] = []
single (x:xs) = [x] : single xs

mymap :: (a0 -> b0) -> [a0] -> [b0]
mymap f xs = [f x | x <- xs]

myfilter :: (a0 -> Bool) -> [a0] -> [a0]
myfilter f xs = [x | x <- xs, f x]

data Day = Mon|Tue|Wed|Thu|Fri|Sat|Sun deriving Show

dayToInt Mon = 0
dayToInt Tue = 1
dayToInt Wed = 2
dayToInt Thu = 3
dayToInt Fri = 4
dayToInt Sat = 5
dayToInt Sun = 6

intToDay 0 = Mon
intToDay 1 = Tue
intToDay 2 = Wed
intToDay 3 = Thu
intToDay 4 = Fri
intToDay 5 = Sat
intToDay 6 = Sun

whichDay :: Day -> Int -> Day
whichDay firstDayOfMonth dayNo = intToDay dayOfWeekNo where 
  dayOfWeekNo = mod (dayNo - 1  + firstDayNo) 7 where
    firstDayNo = dayToInt firstDayOfMonth

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

interNodes :: Tree a -> [a]
interNodes Empty = []
interNodes (Node a Empty Empty) = []
interNodes (Node a l Empty) = (interNodes l) ++ [a]
interNodes (Node a Empty r) = [a] ++ (interNodes r)
interNodes (Node a l r) =
  (interNodes l) ++ [a] ++ (interNodes r)

t :: Tree Int
t = Node 5 (Node 3 (Node 1 Empty Empty)
  (Node 4 Empty Empty))
  (Node 7 Empty
  (Node 9 Empty (Node 11 Empty Empty)))

mirrorTree :: Tree a -> Tree a
mirrorTree Empty = Empty
mirrorTree (Node a Empty Empty) = Node a Empty Empty
mirrorTree (Node a l Empty) = Node a Empty (mirrorTree l)
mirrorTree (Node a Empty r) = Node a (mirrorTree r) Empty
mirrorTree (Node a l r) = Node a (mirrorTree r) (mirrorTree l)

nodesAtLevel :: Tree a -> Int -> [a]
nodesAtLevel Empty _ = []
nodesAtLevel (Node a l r) 1 = [a]
nodesAtLevel (Node a l r) i = (nodesAtLevel l (i-1)) ++ (nodesAtLevel r (i-1))

data FSObject = 
  File {fileName::String} | Dir {dirName::String, files::[FSObject]}
  deriving (Show, Eq)

f :: FSObject
f = Dir "home" 
      [(Dir "d1"
        [(File "f3")
        ,(File "f4")
        ])
      ,(File "f1")
      ,(File "f2")
      ]
emptyDir :: FSObject
emptyDir = Dir "/" []
-- search "f" onlyFile -- Nothing

onlyFile :: FSObject
onlyFile = File "f"
-- search "f" onlyFile -- Just "f"
-- search "f" onlyFile -- Nothing

search :: String -> FSObject -> Maybe String
search name o = getPath name "" o  where
  getPath name path (File fname) 
    | name == fname = Just (path ++"/"++ name)
    | otherwise = Nothing
  getPath name path (Dir dname xs) = msum ( map (getPath name (path ++"/"++ dname)) xs)
-- https://stackoverflow.com/questions/36410971/extracting-the-first-just-value-from-maybe-a