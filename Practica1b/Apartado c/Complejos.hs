module Complejos where

  -- Defnimos nuestro tipo Complejo como uno de los 5 casos posibles, siendo
  --  Cero, Uno e I constantes de 0+0i, 1+0i, 0+1i
  --  Car para representacion cartesiana (a+bi)
  --  Pol para representacion en polares (modulo*fase)
  data Complejo = Cero | Uno | I | Car Double Double | Pol Double Double deriving Show

  -- Funcion que dado un complejo de tipo (Car x y) devuelve su parte real
  real :: Complejo -> Double
  real (Car x y) = x

  -- Funcion que dado un complejo de tipo (Car x y) devuelve su parte imaginaria
  imaginaria :: Complejo -> Double
  imaginaria (Car x y) = y

  -- Funcion que dado cualquier tipo de complejo lo transforma para que se pueda
  --  usar en modo cartesiano, pudiendo llamar asi a las funciones real e imaginaria.
  conversion :: Complejo -> Complejo
  conversion Cero = (Car 0 0)
  conversion Uno = (Car 1 0)
  conversion I = (Car 0 1)
  conversion (Car x y) = (Car x y)
  conversion (Pol x y) = (Car a b) where a = (*) x (cos y)
                                         b = (*) x (sin y)


 {--
      Las funciones de este archivo son equivalentes al apartado anterior, pero
      modificando las funciones que se pueden usar con listas por compresion y
      funciones de haskell de orden superior.
      Esto implica que sumaComplejos, |+, |-, |*, moduloComplejos, convertirACadena,
      convertirListaACadena e idNegativo permanecen iguales.

 --}
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

  -- En lugar de realizar la funcion de manera recursiva, usamos una lista por
  --  comprension usando idNegativo == True como comprobacion
  idListaNegativo :: [Complejo] -> [Complejo]
  idListaNegativo xs = [x | x <- xs, idNegativo x == True]

  -- En lugar de realizar la funcion de manera recursiva, usamos una lista por
  --  comprension.
  -- Esto cambia la lógica de la funcion respecto a apartados anteriores. Donde
  --  antes comprobabamos si la parte real y la parte imaginaria eran iguales a 0,
  --  ahora nos vale con que uno de los valores sea distinto para formar parte de la lista.
  quitarCero :: [Complejo] -> [Complejo]
  quitarCero xs = [x | x <- xs, (real (conversion x) /= 0 || imaginaria (conversion x) /= 0)]

  -- Reimplementamos sortByAscendantModule como una variacion de quicksort,
  --  por lo que ya no necesitamos la funcion auxiliar bubble.
  -- Usamos como condicion la funcion sortByAscendantModule, implementada anteriormente.
  sortByAscendantModule :: [Complejo] -> [Complejo]
  sortByAscendantModule [] = []
  sortByAscendantModule (x:xs) = (sortByAscendantModule [ y | y <- xs, moduloComplejos y <= moduloComplejos x ])
                                 ++ [x] ++
                                 (sortByAscendantModule [ z | z <- xs, moduloComplejos z  > moduloComplejos x ])

  -- En lugar de realizar la funcion de manera recursiva, usamos una lista por
  -- comprension con las 2 listas, realizando asi todas las combinaciones posibles 
  complexDistributed :: [Complejo] -> [Complejo] -> [Complejo]
  complexDistributed xs ys = [x |* y | x <- xs, y <- ys]
