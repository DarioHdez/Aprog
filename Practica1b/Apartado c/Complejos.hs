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
  sumaComplejos (x:xs) =  x |+ (sumaComplejos xs)

  (|+) :: Complejo -> Complejo -> Complejo
  (|+) x y = (Car n m)
                      where n = (+) (real o) (real p)
                            m = (+) (imaginaria o) (imaginaria p)
                            o = conversion x
                            p = conversion y

  (|*) :: Complejo -> Complejo -> Complejo
  (|*) x y = (Car n m)
                  where n = (*) (real o) (real p)
                        m = (*) (imaginaria o) (imaginaria p)
                        o = conversion x
                        p = conversion y

  (|-) :: Complejo -> Complejo -> Complejo
  (|-) x y = (Car n m)
                  where n = (-) (real o) (real p)
                        m = (-) (imaginaria o) (imaginaria p)
                        o = conversion x
                        p = conversion y

  moduloComplejos :: Complejo -> Double
  moduloComplejos x = sqrt y
                            where y = (real z)^^2+(imaginaria z)^^2
                                  z = conversion x

  convertirACadena :: Complejo -> String
  convertirACadena x = show (real y) ++ " + " ++ show (imaginaria y) ++ "i "
                       where y = conversion x

  convertirListaACadena :: [Complejo] -> String
  convertirListaACadena [] = " "
  convertirListaACadena (x:xs) = convertirACadena x ++ ","++ convertirListaACadena xs

  idNegativo :: Complejo -> Bool
  idNegativo x = if (real y) < 0 && (imaginaria y) < 0 then True else False
                 where y = conversion x

  idListaNegativo :: [Complejo] -> [Complejo]
  idListaNegativo xs = [x | x <- xs, idNegativo x == True]

  quitarCero :: [Complejo] -> [Complejo]
  quitarCero xs = [x | x <- xs, (real (conversion x) /= 0 || imaginaria (conversion x) /= 0)]

  sortByAscendantModule :: [Complejo] -> [Complejo]
  sortByAscendantModule [] = []
  sortByAscendantModule (x:xs) = (sortByAscendantModule [ y | y <- xs, moduloComplejos y <= moduloComplejos x ]) ++ [x] ++ (sortByAscendantModule [ z | z <- xs, moduloComplejos z  > moduloComplejos x ])

  complexDistributed :: [Complejo] -> [Complejo] -> [Complejo]
  complexDistributed xs ys = [x |* y | x <- xs, y <- ys]
