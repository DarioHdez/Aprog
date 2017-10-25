module Quick where

  -- Quicksort implementado tal y como viene en las transparencias.
  -- Calcula de forma recursiva los menores y mayores de un elemento,
  --  empezando por el primero de la lista (Que usa como pivote), y a partir
  --  del cual va partiendo la lista hasta que le llega una lista vacia y vuelve
  --  a juntar todos los elementos ya en orden.

  -- En este apartado le añadimos la sentencia error para que falle.
  quicksort :: Ord t => [t] -> [t]
  quicksort [] = error "Fin de lista"
  quicksort (x:xs) = (quicksort [ y | y <- xs, y <= x ])
                     ++ [x] ++
                     (quicksort [ z | z <- xs, z  > x ])
