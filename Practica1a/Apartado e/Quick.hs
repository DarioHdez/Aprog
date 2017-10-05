module Quick where
  import Data.List

  --quicksort [] = []
  {-- quicksort (x:xs) = if xs == [] then error "La lista está vacia"
                    else
                      (quicksort [ y | y <- xs, y <= x ])
                      ++ [x] ++
                      (quicksort [ z | z <- xs, z  > x ]) --}

  quicksort :: Ord t => [t] -> Maybe [t]
  quicksort (x:xs) = if (length xs) < 3 then Nothing
                     else Just (sort (x:xs))
