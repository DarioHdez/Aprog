module Complejos where
  import Data.List

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
  --idListaNegativo [] = []
  --idListaNegativo (x:xs) | (idNegativo x == True) = [x] ++ idListaNegativo xs
    --                     | otherwise = idListaNegativo xs
  idListaNegativo xs = [x | x <- xs, idNegativo x == True]

  quitarCero :: [Complejo] -> [Complejo]
  --quitarCero [] = []
  --quitarCero (x:xs) | (real y) == 0 && (imaginaria y) == 0 = quitarCero xs
              --      | otherwise = [x] ++ quitarCero xs
                    --  where y = conversion x
  quitarCero xs = [x | x <- xs, real (conversion x) /= 0, imaginaria (conversion x) /= 0]

  sortByAscendantModule :: [Complejo] -> [Complejo]
--  sortByAscendantModule [x] = [x]
--  sortByAscendantModule (x:xs) = bubble x (sortByAscendantModule xs)

  --bubble :: Complejo -> [Complejo] -> [Complejo]
  --bubble y [] = [y]
  --bubble y (x:xs) | moduloComplejos y > moduloComplejos x = [x] ++ bubble y xs
                --  | otherwise = [y] ++ [x] ++ xs

  complexDistributed :: [Complejo] -> [Complejo] -> [Complejo]
  complexDistributed [] [] = []
  complexDistributed xs [] = []
  complexDistributed [] ys = []
  complexDistributed [x] ys = runYs x ys
  complexDistributed (x:xs) ys = (complexDistributed xs ys) ++ (runYs x ys)

  runYs :: Complejo -> [Complejo] -> [Complejo]
  runYs x [] = []
  runYs x ys = [(x |* (head ys))] ++ (runYs x (tail ys))
