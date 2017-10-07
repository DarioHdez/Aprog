module Parcial1 where

  calcular :: (Int -> Int -> Int) -> Int -> [Int] -> Int
  calcular f e l = calcular' l
    where calcular' [] = e
          calcular' (x:xs) = f x (calcular' xs)

  res1 = calcular (+) 0 [1..4]
  res2 = calcular (*) 1 [1..4]
  res3 = calcular (*) 0 [1..]
  res4 = calcular (-) 0 [1..4]
  res5 = calcular f 0 [1..4]
    where f x = (*2) . (+) x

  calcular' :: (Int -> Int -> Int) -> Int -> [Int] -> Int
  calcular' f e l
      | l <= []   = e
      | otherwise = f x (calcular' f e xs)
      where x = head l
            xs = tail l
