module Complejos where

 type Complejo = (Double,Double)

 sumaComplejos :: [Complejo] -> Complejo
 sumaComplejos [] = (0,0)
 sumaComplejos (x:xs) = sumaComplejos_base x (sumaComplejos xs)

 sumaComplejos_base :: Complejo -> Complejo -> Complejo
 sumaComplejos_base x y = (n,m)
                     where n = (+) (fst x) (fst y)
                           m = (+) (snd x) (snd y)

 multiplicarComplejos :: Complejo -> Complejo -> Complejo
 multiplicarComplejos x y = (n,m)
                            where n = (*) (fst x) (fst y)
                                  m = (*) (snd x) (snd y)

 restarComplejos :: Complejo -> Complejo -> Complejo
 restarComplejos x y = (n,m)
                       where n = (-) (fst x) (fst y)
                             m = (-) (snd x) (snd y)

 moduloComplejos :: Complejo -> Double
 moduloComplejos x = sqrt y where y = (fst x)^^2+(snd x)^^2

 convertirACadena :: Complejo -> String
 convertirACadena x =  show (fst x) ++ " + " ++ show (snd x) ++ "i "

 convertirListaACadena :: [Complejo] -> String
 convertirListaACadena [] = " "
 convertirListaACadena (x:xs) = convertirACadena x ++ convertirListaACadena xs

 idNegativo :: Complejo -> Bool
 idNegativo x = if (fst x) < 0 && (snd x) < 0 then True
                else False

 idListaNegativo :: [Complejo] -> [Complejo]
 idListaNegativo [] = []
 idListaNegativo (x:xs) | (idNegativo x == True) = [x] ++ idListaNegativo xs
                        | otherwise = idListaNegativo xs

 quitarCero :: [Complejo] -> [Complejo]
 quitarCero [] = []
 quitarCero (x:xs) | (fst x) == 0 && (snd x) == 0 = quitarCero xs
                   | otherwise = [x] ++ quitarCero xs

 sortByAscendantModule :: [Complejo] -> [Complejo]
 sortByAscendantModule [x] = [x]
 sortByAscendantModule (x:xs) = bubble x (sortByAscendantModule xs)

 bubble :: Complejo -> [Complejo] -> [Complejo]
 bubble y [] = [y]
 bubble y (x:xs) | moduloComplejos y > moduloComplejos x = [x] ++ bubble y xs
                 | otherwise = [y] ++ [x] ++ xs

 complexDistributed :: [Complejo] -> [Complejo] -> [Complejo]
 complexDistributed [] [] = []
 complexDistributed xs [] = []
 complexDistributed [] ys = []
 complexDistributed [x] ys = runYs x ys
 complexDistributed (x:xs) ys = (complexDistributed xs ys) ++ (runYs x ys)

 runYs :: Complejo -> [Complejo] -> [Complejo]
 runYs x [] = []
 runYs x ys = [(multiplicarComplejos x (head ys))] ++ (runYs x (tail ys))
