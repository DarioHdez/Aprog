module Apartado_k where



-- https://hackage.haskell.org/package/base-4.6.0.1/docs/src/Data-Foldable.html#Foldable
 --  -- | The 'sum' function computes the sum of the numbers of a structure.
 -- sum :: (Foldable t, Num a) => t a -> a
 -- sum = getSum . foldMap Sum

-- https://hackage.haskell.org/package/base-4.10.0.0/docs/src/GHC.List.html#sum
 -- -- | The 'sum' function computes the sum of a finite list of numbers.
 -- sum                     :: (Num a) => [a] -> a
 -- {-# INLINE sum #-}
 -- sum                     =  foldl (+) 0

 sum1 :: [Integer] -> Integer
 sum1 = foldl (+) 0
