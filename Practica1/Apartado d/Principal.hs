module Principal where
  import Quick
  import GHC.List

  -- Ejercicio d
  miReplicate :: Int -> a -> [a]
  miReplicate 1 x = [x]
  miReplicate n x = [x] ++ miReplicate (n-1) x

  -- replicate original:
  --  replicate n x = take n (repeat x)

  miFilter :: (a -> Bool) -> [a] -> [a]
  miFilter _pred []    = []
  miFilter pred (x:xs)
          | pred x         = x : filter pred xs
          | otherwise      = filter pred xs

  main :: IO()
  main = do print (miReplicate 3 "Wololo")
            print (miReplicate 3 True)
            print (miReplicate 3 5)
            print (miFilter (>5) [5,6,7,8])
