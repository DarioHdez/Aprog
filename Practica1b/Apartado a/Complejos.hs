module Complejos where

  --data Complejo = Complejo { real :: Double ,
    --                         imaginario :: Double
      --                     } deriving (Show)

  --sumaComplejo :: Complejo a => a -> a -> a
  --sumaComplejo x y = (n,m) where n = (+) (fst x) (fst y)
    --                             m = (+) (snd x) (snd y)


 a :: (Double, Double)
 a = (1,3)

 b :: (Double,Double)
 b = (2,5)

 sumaComplejos :: (Double,Double) -> (Double,Double) -> (Double,Double)
 sumaComplejos x y = (n,m)
                     where n = (+) (fst x) (fst y)
                           m = (+) (snd x) (snd y)

 multiplicarComplejos :: (Double,Double) -> (Double,Double) -> (Double,Double)
 multiplicarComplejos x y = (n,m)
                            where n = (*) (fst x) (fst y)
                                  m = (*) (snd x) (snd y)

 restarComplejos :: (Double,Double) -> (Double,Double) -> (Double,Double)
 restarComplejos x y = (n,m)
                       where n = (-) (fst x) (fst y)
                             m = (-) (snd x) (snd y)

 moduloComplejos :: (Double,Double) -> Double
 moduloComplejos x = sqrt y where y = (fst x)^^2+(snd x)^^2

 convertirACadena :: (Double,Double) -> String
 convertirACadena x = " (" ++ show (fst x) ++ "," ++ show (snd x) ++ ") "

 convertirListaACadena :: [(Double,Double)] -> String
 convertirListaACadena [] = " "
 convertirListaACadena (x:xs) = convertirACadena x ++ convertirListaACadena xs

 idNegativo :: (Double,Double) -> Bool
 idNegativo x = if (fst x) < 0 && (snd x) < 0 then True
                else False

 idListaNegativo :: [(Double,Double)] -> [(Double,Double)]
 idListaNegativo [] = []
 idListaNegativo (x:xs) | (idNegativo x == True) = [x] ++ idListaNegativo xs
                        | otherwise = idListaNegativo xs

 quitarCero :: [(Double,Double)] -> [(Double,Double)]
 quitarCero [] = []
 quitarCero (x:xs) | (fst x) == 0 && (snd x) == 0 = quitarCero xs
                   | otherwise = [x] ++ quitarCero xs

 sortByAscendantModule :: [(Double,Double)] -> [(Double,Double)]
 sortByAscendantModule [x] = [x]
 sortByAscendantModule (x:xs) = bubble x (sortByAscendantModule xs)

 bubble :: (Double,Double) -> [(Double,Double)] -> [(Double,Double)]
 bubble y [] = [y]
 bubble y (x:xs) | moduloComplejos y > moduloComplejos x = [x] ++ bubble y xs
                 | otherwise = [y] ++ [x] ++ xs

 complexDistributed :: [(Double,Double)] -> [(Double,Double)] -> [(Double,Double)]
 complexDistributed [] [] = []
 complexDistributed xs [] = []
 complexDistributed [] ys = []
 complexDistributed [x] ys = runYs x ys
 complexDistributed (x:xs) ys = (complexDistributed xs ys) ++ (runYs x ys)

 runYs :: (Double,Double) -> [(Double,Double)] -> [(Double,Double)]
 runYs x [] = []
 runYs x ys = [(multiplicarComplejos x (head ys))] ++ (runYs x (tail ys))
