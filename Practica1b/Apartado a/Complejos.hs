module Complejos where

 -- Defnimos nuestro tipo Complejo como una tupla de 2 Doubles.
 type Complejo = (Double,Double)

 -- Funcion que suma una lista de complejos.
 -- Recibe una lista de Complejos y devuelve un complejo cuyo valor es
 --  la suma de toda la lista.
 -- La recursion funciona de la siguiente manera: Se va desplegando la lista
 --  termino a termino y cuando llega al ultimo suma todos al volver de la recursion.
 sumaComplejos :: [Complejo] -> Complejo
 sumaComplejos [] = (0,0) -- Caso base de la recursividad
 sumaComplejos (x:xs) = sumaComplejos_base x (sumaComplejos xs) -- Llamada recursiva

 -- Funcion que suma 2 complejos.
 -- Recibe 2 numeros complejos y devuelve uno cuyo valor es la suma.
 -- La parte real se calcula sumando las 2 partes reales y lo mismo para la parte
 --   imaginaria.
 sumaComplejos_base :: Complejo -> Complejo -> Complejo
 sumaComplejos_base x y = (n,m)
                     where n = (+) (fst x) (fst y)
                           m = (+) (snd x) (snd y)

 -- Funcion que multiplica 2 complejos.
 -- Recibe 2 numeros complejos y devuelve uno cuyo valor es la multiplicacion.
 -- La parte real se calcula multiplicando las 2 partes reales y lo mismo para
 --  la parte imaginaria.
 multiplicarComplejos :: Complejo -> Complejo -> Complejo
 multiplicarComplejos x y = (n,m)
                            where n = (*) (fst x) (fst y)
                                  m = (*) (snd x) (snd y)

 -- Funcion que resta 2 complejos.
 -- Recibe 2 numeros complejos y devuelve uno cuyo valor es la resta del
 --  primero menos el segundo.
 -- La parte real se calcula restando las 2 partes reales y lo mismo para la parte
 --   imaginaria.
 restarComplejos :: Complejo -> Complejo -> Complejo
 restarComplejos x y = (n,m)
                       where n = (-) (fst x) (fst y)
                             m = (-) (snd x) (snd y)

 -- Funcion que calcula el modulo de un complejo.
 -- Recibe un numero complejo y devuelve un double con el valor del modulo.
 -- El valor del modulo de un numero complejo se calcula segun la formula.
 moduloComplejos :: Complejo -> Double
 moduloComplejos x = sqrt y where y = (fst x)^^2+(snd x)^^2

 -- Funcion que convierte y muestra por pantalla un complejo en modo x +yi
 -- Recibe un numero complejo y devuelve una cadena.
 convertirACadena :: Complejo -> String
 convertirACadena x =  show (fst x) ++ " + " ++ show (snd x) ++ "i "

 -- Funcion que convierte y muestra por pantalla una lista de complejos.
 -- Recibe una lista de complejos y devuelve una cadena.
 -- La recursion separa cada complejo de la lista y usa la funcion
 --  convertirACadena para imprimir cada numero por separado.
 convertirListaACadena :: [Complejo] -> String
 convertirListaACadena [] = " " -- Caso base
 convertirListaACadena (x:xs) = convertirACadena x ++ convertirListaACadena xs

 -- Funcion que indica con un Boolean si un numero complejo es negativo o no.
 -- Recibe un numero complejo y comprueba para la parte real e imaginaria si ambas
 --  son menores que 0. En ese caso devuelve True, si alguna de las partes es
 --  mayor que 0, la funcion devuelve False.
 idNegativo :: Complejo -> Bool
 idNegativo x = if (fst x) < 0 && (snd x) < 0 then True
                else False

 -- Funcion que dada una lista de complejos devuelve la misma lista pero solo con
 --  los numeros negativos en ella.
 -- Recibe una lista y devuelve otra lista. El resultado se calcula de manera recursiva,
 --  pasando cada elemento a idNegativo para comprobar si es parte de la solucion o no.
 idListaNegativo :: [Complejo] -> [Complejo]
 idListaNegativo [] = [] -- Caso base
 idListaNegativo (x:xs) | (idNegativo x == True) = [x] ++ idListaNegativo xs -- x pertenece a la solucion
                        | otherwise = idListaNegativo xs -- x no pertenece a la solucion

 -- Funcion que dada una lista de complejos devuelve la misma lista pero solo con
 --  los elementos distintos de 0.
 -- Recibe una lista y devuelve otra lista. El resultado se calcula de manera recursiva
 --  para cada elemento, comprobando si su parte real e imaginaria son iguales a 0.
 quitarCero :: [Complejo] -> [Complejo]
 quitarCero [] = [] -- Caso base
 quitarCero (x:xs) | (fst x) == 0 && (snd x) == 0 = quitarCero xs -- Elemento igual a 0.
                   | otherwise = [x] ++ quitarCero xs -- se va concatenando con la recursion.

 -- Funcion que dada una lista de complejos devuelve la misma lista, pero con los
 --  elementos ordenados por modulo ascendente.
 -- Recibe una lista y devuelve otra lista. El resultado se calcula de manera recursiva.
 -- El caso base se cuenta como un solo elemento de la lista, a partir del cual la recursión
 --  vuelve "hacia arriba" e inserta cada elemento ya en orden mediante la funcion auxiliar bubble.
 sortByAscendantModule :: [Complejo] -> [Complejo]
 sortByAscendantModule [x] = [x] -- Caso base
 sortByAscendantModule (x:xs) = bubble x (sortByAscendantModule xs)

 -- Funcion auxiliar que dado un complejo y una lista de complejos, devuelve la lista, pero
 --  el elemento insertado en orden. Si la lista que recibe como argumento no esta
 --  ya ordenada el algoritmo no funciona.
 -- Como es solo la lista lo que se recorre recursivamente, el caso base implica
 --  que se ha recorrido toda la lista y por lo tanto el nuevo elemento es el mayor.
 bubble :: Complejo -> [Complejo] -> [Complejo]
 bubble y [] = [y] -- Caso base
 bubble y (x:xs) | moduloComplejos y > moduloComplejos x = [x] ++ bubble y xs
                    -- Se cumple que y > x, por lo tanto se forma la lista [x,bubble y xs]
                 | otherwise = [y] ++ [x] ++ xs -- y < x => [y,x,xs]

 -- Funcion que dadas 2 listas de numeros complejos devuelve una lista con todas las
 --  combinaciones de multiplicaciones posibles, es decir, la propiedad distributiva.
 --  (a+b)(c+d) = ac+ad+bc+bd
 -- Recibe una lista y devuelve otra lista. El resultado se calcula de forma recursiva.
 -- Se comprueban distintos casos bases para que no haya problemas a la hora de
 --  usar listas de distinta longitud. Para cada elemento de la primera lista,
 --  usa la funcion auxiliar runYs, que multiplica ese elemento con toda la segunda lista.
 complexDistributed :: [Complejo] -> [Complejo] -> [Complejo]
 complexDistributed [] [] = [] -- []*[] = []
 complexDistributed xs [] = [] -- [a,b]*[] = []
 complexDistributed [] ys = [] -- []*[a,b] = []
 complexDistributed [x] ys = runYs x ys -- Caso base para un solo elemento: llamada  a la funcion auxiliar.
 complexDistributed (x:xs) ys = (complexDistributed xs ys) ++ (runYs x ys)
    -- Recursion sobre el primer elemento de la lista.

 -- Funcion auxiliar que dado un complejo y una lista de complejos devuelve una lista
 --  con el complejo multiplicado por cada elemento de la lista.
 -- El caso base comprueba solo que la lista este vacia, pues es sobre la que se
 --  realiza la recursion.
 -- Se llama a la funcion multiplicarComplejos con el elemento y el head de la lista
 --  y se concatena con la recursion sobre el resto de la lista.
 runYs :: Complejo -> [Complejo] -> [Complejo]
 runYs x [] = [] -- Caso basse.
 runYs x ys = [(multiplicarComplejos x (head ys))] ++ (runYs x (tail ys)) -- Recursion.
