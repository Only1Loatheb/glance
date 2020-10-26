quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  quicksort (filter (< x) xs) ++
  [ x ] ++
  quicksort (filter (>= x) xs)

quicksortLc :: (Ord a) => [a] -> [a]
quicksortLc [] = []
quicksortLc (x:xs) =
    let smallerSorted = quicksortLc [a | a <- xs, a <= x]
        biggerSorted  = quicksortLc [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted