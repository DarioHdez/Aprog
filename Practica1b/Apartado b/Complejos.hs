module Complejos where

  data Complejo = Cero | Uno | I | Car Double Double | Pol Double Double deriving Show

  real :: Complejo -> Double
  real (Car x y) = x

  imaginaria :: Complejo -> Double
  imaginaria (Car x y) = y

  conversion :: Complejo -> Complejo
  conversion Cero = (Car 0 0)
  conversion Uno = (Car 1 0)
  conversion I = (Car 0 1)
  conversion (Car x y) = (Car x y)
  conversion (Pol x y) = (Car a b) where a = (*) x (cos y)
                                         b = (*) x (sin y)

  sumaComplejos :: [Complejo] -> Complejo
  sumaComplejos [] = (Car 0 0)
  sumaComplejos (x:xs) =  (conversion x) +++ (sumaComplejos xs)

  (+++) :: Complejo -> Complejo -> Complejo
  (+++) x y = (Car n m)
                      where n = (+) (real x) (real y)
                            m = (+) (imaginaria x) (imaginaria y)
