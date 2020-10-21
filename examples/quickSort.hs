quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  quicksort (filter (< x) xs) ++
  [ x ] ++
  quicksort (filter (>= x) xs)