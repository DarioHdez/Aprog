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
 idListaNegativo (x:xs) = [x | ] 
