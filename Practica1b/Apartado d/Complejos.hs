{-# LANGUAGE GADTs, KindSignatures, RankNTypes, FlexibleContexts, FlexibleInstances #-}

module Complejos where

  -- Definimos nuestro tipo Complejo a como un tipo de dato polimorfico para
  --  operaciones validas de la clase Floating. Las representaciones siguen siendo
  --  las mismas que en apartados anteriores, pero ahora necesitamos instanciar
  --  Show, Eq y Ord en nuestra clase
  data Complejo a where
    Car, Pol :: (Floating a) => a -> a -> Complejo a
    Cero, Uno, I :: (Floating a) => Complejo a

  -- Con esta instance implementamos la manera de mostrar numeros complejos por
  --  pantalla, lo que nos ahorra la funcion convertirACadena y convertirListaACadena
  instance (Floating a, Show a, Eq a) => Show (Complejo a) where
    show c = show' (real (conversion c)) ++ "+" ++ show' (imaginaria  (conversion c)) ++ "i"
            where show' n
                     | signum n == (-1) = "(" ++ show n ++ ")"
                     | otherwise = show n

  -- Con esta instance implementamos la manera de comparar complejos.
  -- Dos numeros complejos son iguales cuando su parte real y su parte imaginaria
  --  son iguales.
  instance (Floating a, Eq a) => Eq (Complejo a) where
       c1 == c2 = (real (conversion c1) == real (conversion c2)) && (imaginaria (conversion c2) == imaginaria (conversion c1))

  -- Con esta instancia implementamos la manera de ordenar complejos.
  -- Para saber cuando un complejo es mayor o menor, comparamos su modulo.
  instance (Floating a, Eq a, Ord a) => Ord (Complejo a) where
         compare c1 c2 =  compare (moduloComplejos c1) (moduloComplejos c2)

  {--
    Todo el codigo correspondiente a las funciones de este apartado es igual
    al implementado en apartados anteriores, por lo que por limpieza y simplicidad
    los comentarios no se añadiran.  

  --}

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

  sumaComplejos :: (Floating a) => [Complejo a] -> Complejo a
  sumaComplejos [] = (Car 0 0)
  sumaComplejos (x:xs) =  x |+ (sumaComplejos xs)

  (|+) :: (Floating a) => Complejo a -> Complejo a -> Complejo a
  (|+) x y = (Car n m)
                      where n = (+) (real o) (real p)
                            m = (+) (imaginaria o) (imaginaria p)
                            o = conversion x
                            p = conversion y

  (|*) :: (Floating a) => Complejo a -> Complejo a -> Complejo a
  (|*) x y = (Car n m)
                  where n = (*) (real o) (real p)
                        m = (*) (imaginaria o) (imaginaria p)
                        o = conversion x
                        p = conversion y

  (|-) :: (Floating a) => Complejo a -> Complejo a -> Complejo a
  (|-) x y = (Car n m)
                  where n = (-) (real o) (real p)
                        m = (-) (imaginaria o) (imaginaria p)
                        o = conversion x
                        p = conversion y

  moduloComplejos :: (Floating a) => Complejo a -> a
  moduloComplejos x = sqrt y
                            where y = (real z)^^2+(imaginaria z)^^2
                                  z = conversion x

  idNegativo :: (Floating a, Ord a) => Complejo a -> Bool
  idNegativo x = if (real y) < 0 && (imaginaria y) < 0 then True else False
                    where y = conversion x

  idListaNegativo :: (Floating a, Ord a) => [Complejo a] -> [Complejo a]
  idListaNegativo xs = [x | x <- xs , idNegativo x == True]

  quitarCero :: (Floating a, Ord a) => [Complejo a] -> [Complejo a]
  quitarCero xs = [x | x <- xs, (real (conversion x) /= 0 || imaginaria (conversion x) /= 0)]

  sortByAscendantModule :: (Floating a, Ord a) => [Complejo a] -> [Complejo a]
  sortByAscendantModule [] = []
  sortByAscendantModule (x:xs) = (sortByAscendantModule [ y | y <- xs, moduloComplejos y <= moduloComplejos x ])
                                 ++ [x] ++
                                 (sortByAscendantModule [ z | z <- xs, moduloComplejos z  > moduloComplejos x ])

  complexDistributed :: (Floating a) => [Complejo a] -> [Complejo a] -> [Complejo a]
  complexDistributed xs ys = [x |* y | x <- xs, y <- ys]
