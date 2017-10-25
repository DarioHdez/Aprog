module Principal where
  import Quick
  import Data.List
  import GHC.List

  -- Funcion recursiva que dado un entero n y un elemento x,
  --  repite en una lista n veces el elemento x.
  miReplicate :: Int -> a -> [a]
  miReplicate 1 x = [x] -- Caso base
  miReplicate n x = [x] ++ miReplicate (n-1) x

  -- replicate original:
  --  replicate n x = take n (repeat x)

  -- El codigo de miFilter es igual al original del Prelude.
  miFilter :: (a -> Bool) -> [a] -> [a]
  miFilter _pred []    = []
  miFilter pred (x:xs)
          | pred x         = x : filter pred xs
          | otherwise      = filter pred xs

  main :: IO()
  main = do print (quicksort [3,1,5,9,2,6])
            print (quicksort [1,3,2])
            print (sort [5,2,9,8,1])
            print (miReplicate 3 "Wololo")
            print (miReplicate 3 True)
            print (miReplicate 3 5)
            print (miFilter (>5) [5,6,7,8])
