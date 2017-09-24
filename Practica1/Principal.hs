module Principal where
  import Quick
  import Data.List

  var :: [Integer]
  var = quicksort [3,1,5,9,2,6]

  var2 :: [Integer]
  var2 = quicksort [1,3,2]

  var3 :: [Integer]
  var3 = sort [5,2,9,8,1]

  main = do print var
            print var2
            print var3
