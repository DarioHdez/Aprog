module Quick where

  quicksort :: Ord t => [t] -> [t]

  --quicksort [] = []
  quicksort (x:xs) = if xs == [] then error "La lista está vacia"
                    else
                      (quicksort [ y | y <- xs, y <= x ])
                      ++ [x] ++
                      (quicksort [ z | z <- xs, z  > x ])
