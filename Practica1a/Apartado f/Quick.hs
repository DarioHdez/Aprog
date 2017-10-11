module Quicksort where


  quicksort :: Ord t => [t] -> [t]
  quicksort [] = []
  quicksort (x:xs) = quicksort (filter (<=x) xs) ++ [x] ++ quicksort (filter (>x) xs)
