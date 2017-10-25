module Principal where
  import Quick
  import Data.List
  import GHC.List

  -- Pruebas realizadas sobre quicksort y sort

  main :: IO()
  main = do print (quicksort [3,1,5,9,2,6])
            print (quicksort [1,3,2])
            print (sort [5,2,9,8,1])
