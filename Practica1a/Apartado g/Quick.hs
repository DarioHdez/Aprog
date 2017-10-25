module Quicksort where

  -- Quicksort implementado sin listas por comprension usando la funcion
  --  filter. La recursividad se mantiene, pero para sacar los numeros menores
  --  y mayores usamos filter con <= y >
  quicksort :: Ord t => [t] -> [t]
  quicksort [] = []
  quicksort (x:xs) = quicksort (filter (<=x) xs) ++ [x] ++ quicksort (filter (>x) xs)
