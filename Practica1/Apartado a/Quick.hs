module Quick where

  quicksort :: Ord t => [t] -> [t]

  quicksort [] = []
  quicksort (x:xs) = (quicksort [ y | y <- xs, y <= x ]) ++ [x] ++ (quicksort [ z | z <- xs, z  > x ])
