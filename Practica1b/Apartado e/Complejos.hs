{-# LANGUAGE GADTs, KindSignatures, RankNTypes, FlexibleContexts, FlexibleInstances #-}

module Complejos where

  data Complejo a where
    Car, Pol :: (Floating a) => a -> a -> Complejo a
    Cero, Uno, I :: (Floating a) => Complejo a

  instance (Floating a, Show a, Eq a) => Show (Complejo a) where
    show c = show' (real (conversion c)) ++ "+" ++ show' (imaginaria  (conversion c)) ++ "i"
            where show' n
                     | signum n == (-1) = "(" ++ show n ++ ")"
                     | otherwise = show n

  instance (Floating a, Eq a) => Eq (Complejo a) where
       c1 == c2 = (real (conversion c1) == real (conversion c2)) && (imaginaria (conversion c2) == imaginaria (conversion c1))

  instance (Floating a, Eq a, Ord a) => Ord (Complejo a) where
         compare c1 c2 =  compare (moduloComplejos c1) (moduloComplejos c2)

  real :: (Floating a) => Complejo a -> a
  real (Car x y) = x

  imaginaria :: (Floating a) => Complejo a -> a
  imaginaria (Car x y) = y

  conversion :: (Floating a) => Complejo a -> Complejo a
  conversion Cero = (Car 0 0)
  conversion Uno = (Car 1 0)
  conversion I = (Car 0 1)
  conversion (Car x y) = (Car x y)
  conversion (Pol x y) = (Car a b) where a = (*) x (cos y)
                                         b = (*) x (sin y)

  moduloComplejos :: (Floating a) => Complejo a -> a
  moduloComplejos x = sqrt y
                            where y = (real z)^^2+(imaginaria z)^^2
                                  z = conversion x

  sortByAscendantModule :: (Floating a, Ord a) => [Complejo a] -> [Complejo a]
  sortByAscendantModule [] = []
  sortByAscendantModule (x:xs) = (sortByAscendantModule [ y | y <- xs, moduloComplejos y <= moduloComplejos x ])
                                 ++ [x] ++
                                 (sortByAscendantModule [ z | z <- xs, moduloComplejos z  > moduloComplejos x ])
