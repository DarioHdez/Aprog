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
  conversion Cero = (Car 0 0) -- Cero = 0+0i
  conversion Uno = (Car 1 0)  -- Uno  = 1+0i
  conversion I = (Car 0 1)    --  I   = 0+1i
  conversion (Car x y) = (Car x y) -- cartesianas
  conversion (Pol x y) = (Car a b) where a = (*) x (cos y) -- Polares
                                         b = (*) x (sin y)

{--
      Las funciones de este archivo son iguales a las del apartado a, pero
      adaptandolas al nuevo tipo, esto implica que antes de realizar cualquier
      operacion, se hace una conversion del numero complejo para tartar siempre
      en representacion cartesiana. Por este motivo las funciones tienen alguna
      linea mas que en el apartado anterior.
--}


  -- Funcion que suma una lista de complejos.
  -- Recibe una lista de Complejos y devuelve un complejo cuyo valor es
  --  la suma de toda la lista.
  -- La recursion funciona de la siguiente manera: Se va desplegando la lista
  --  termino a termino y cuando llega al ultimo suma todos al volver de la recursion.
  sumaComplejos :: [Complejo] -> Complejo
  sumaComplejos [] = (Car 0 0)
  sumaComplejos (x:xs) =  x |+ (sumaComplejos xs)

  -- Funcion que suma 2 complejos.
  -- Recibe 2 numeros complejos y devuelve uno cuyo valor es la suma.
  -- La parte real se calcula sumando las 2 partes reales y lo mismo para la parte
  --   imaginaria.
  (|+) :: Complejo -> Complejo -> Complejo
  (|+) x y = (Car n m)
                      where n = (+) (real o) (real p)
                            m = (+) (imaginaria o) (imaginaria p)
                            o = conversion x
                            p = conversion

  -- Funcion que multiplica 2 complejos.
  -- Recibe 2 numeros complejos y devuelve uno cuyo valor es la multiplicacion.
  -- La parte real se calcula multiplicando las 2 partes reales y lo mismo para
  --  la parte imaginaria.
  (|*) :: Complejo -> Complejo -> Complejo
  (|*) x y = (Car n m)
                  where n = (*) (real o) (real p)
                        m = (*) (imaginaria o) (imaginaria p)
                        o = conversion x
                        p = conversion y

  -- Funcion que resta 2 complejos.
  -- Recibe 2 numeros complejos y devuelve uno cuyo valor es la resta del
  --  primero menos el segundo.
  -- La parte real se calcula restando las 2 partes reales y lo mismo para la parte
  --   imaginaria.
  (|-) :: Complejo -> Complejo -> Complejo
  (|-) x y = (Car n m)
                  where n = (-) (real o) (real p)
                        m = (-) (imaginaria o) (imaginaria p)
                        o = conversion x
                        p = conversion y

  -- Funcion que calcula el modulo de un complejo.
  -- Recibe un numero complejo y devuelve un double con el valor del modulo.
  -- El valor del modulo de un numero complejo se calcula segun la formula.
  moduloComplejos :: Complejo -> Double
  moduloComplejos x = sqrt y
                            where y = (real z)^^2+(imaginaria z)^^2
                                  z = conversion x

  -- Funcion que convierte y muestra por pantalla un complejo en modo x +yi
  -- Recibe un numero complejo y devuelve una cadena.
  convertirACadena :: Complejo -> String
  convertirACadena x = show (real y) ++ " + " ++ show (imaginaria y) ++ "i "
                       where y = conversion x

  -- Funcion que convierte y muestra por pantalla una lista de complejos.
  -- Recibe una lista de complejos y devuelve una cadena.
  -- La recursion separa cada complejo de la lista y usa la funcion
  --  convertirACadena para imprimir cada numero por separado.
  convertirListaACadena :: [Complejo] -> String
  convertirListaACadena [] = " "
  convertirListaACadena (x:xs) = convertirACadena x ++ ","++ convertirListaACadena xs

  -- Funcion que indica con un Boolean si un numero complejo es negativo o no.
  -- Recibe un numero complejo y comprueba para la parte real e imaginaria si ambas
  --  son menores que 0. En ese caso devuelve True, si alguna de las partes es
  --  mayor que 0, la funcion devuelve False.
  idNegativo :: Complejo -> Bool
  idNegativo x = if (real y) < 0 && (imaginaria y) < 0 then True else False
                 where y = conversion x

  -- Funcion que dada una lista de complejos devuelve la misma lista pero solo con
  --  los numeros negativos en ella.
  -- Recibe una lista y devuelve otra lista. El resultado se calcula de manera recursiva,
  --  pasando cada elemento a idNegativo para comprobar si es parte de la solucion o no.
  idListaNegativo :: [Complejo] -> [Complejo]
  idListaNegativo [] = []
  idListaNegativo (x:xs) | (idNegativo x == True) = [x] ++ idListaNegativo xs
                         | otherwise = idListaNegativo xs

  -- Funcion que dada una lista de complejos devuelve la misma lista pero solo con
  --  los elementos distintos de 0.
  -- Recibe una lista y devuelve otra lista. El resultado se calcula de manera recursiva
  --  para cada elemento, comprobando si su parte real e imaginaria son iguales a 0.
  quitarCero :: [Complejo] -> [Complejo]
  quitarCero [] = []
  quitarCero (x:xs) | (real y) == 0 && (imaginaria y) == 0 = quitarCero xs
                    | otherwise = [x] ++ quitarCero xs
                      where y = conversion x

{--
      Las siguientes funciones tienen el mismo codigo que el apartado a.
      Esto se debe a que las operaciones se realizan mediante otras funciones y
      no se necesita de hacer las conversiones en estas funciones.
      Esto demuestra la modularidad de la implementación.
--}

  -- Funcion que dada una lista de complejos devuelve la misma lista, pero con los
  --  elementos ordenados por modulo ascendente.
  -- Recibe una lista y devuelve otra lista. El resultado se calcula de manera recursiva.
  -- El caso base se cuenta como un solo elemento de la lista, a partir del cual la recursión
  --  vuelve "hacia arriba" e inserta cada elemento ya en orden mediante la funcion auxiliar bubble.
  sortByAscendantModule :: [Complejo] -> [Complejo]
  sortByAscendantModule [x] = [x]
  sortByAscendantModule (x:xs) = bubble x (sortByAscendantModule xs)

  -- Funcion auxiliar que dado un complejo y una lista de complejos, devuelve la lista, pero
  --  el elemento insertado en orden. Si la lista que recibe como argumento no esta
  --  ya ordenada el algoritmo no funciona.
  -- Como es solo la lista lo que se recorre recursivamente, el caso base implica
  --  que se ha recorrido toda la lista y por lo tanto el nuevo elemento es el mayor.
  bubble :: Complejo -> [Complejo] -> [Complejo]
  bubble y [] = [y]
  bubble y (x:xs) | moduloComplejos y > moduloComplejos x = [x] ++ bubble y xs
                  | otherwise = [y] ++ [x] ++ xs

  -- Funcion que dadas 2 listas de numeros complejos devuelve una lista con todas las
  --  combinaciones de multiplicaciones posibles, es decir, la propiedad distributiva.
  --  (a+b)(c+d) = ac+ad+bc+bd
  -- Recibe una lista y devuelve otra lista. El resultado se calcula de forma recursiva.
  -- Se comprueban distintos casos bases para que no haya problemas a la hora de
  --  usar listas de distinta longitud. Para cada elemento de la primera lista,
  --  usa la funcion auxiliar runYs, que multiplica ese elemento con toda la segunda lista.
  complexDistributed :: [Complejo] -> [Complejo] -> [Complejo]
  complexDistributed [] [] = []
  complexDistributed xs [] = []
  complexDistributed [] ys = []
  complexDistributed [x] ys = runYs x ys
  complexDistributed (x:xs) ys = (complexDistributed xs ys) ++ (runYs x ys)

  -- Funcion auxiliar que dado un complejo y una lista de complejos devuelve una lista
  --  con el complejo multiplicado por cada elemento de la lista.
  -- El caso base comprueba solo que la lista este vacia, pues es sobre la que se
  --  realiza la recursion.
  -- Se llama a la funcion multiplicarComplejos con el elemento y el head de la lista
  --  y se concatena con la recursion sobre el resto de la lista.
  runYs :: Complejo -> [Complejo] -> [Complejo]
  runYs x [] = []
  runYs x ys = [(x |* (head ys))] ++ (runYs x (tail ys))
