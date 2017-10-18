module Ej3 where

data Complejo = Comp a (b) | x ::: b

-- CÓDIGO

c1 :: Complejo
c1=Comp 3 (-4)

c2 :: Complejo
c2=3 ::: 4

c3 :: Complejo
c3= 1 ::: 1

c4::Complejo
c4=3

c5::Complejo
c5=3.2

instance Show Complejo where -- CÓDIGO

instance Eq Complejo where -- CÓDIGO

instance Num Complejo where -- CÓDIGO

instance Fractional Complejo where -- CÓDIGO

----------------------------------------------------------------

-- Tipo polimórfico
data Complejo' n where -- uso de GADTs
    Comp' :: (Fractional n) => n -> n -> Complejo' n
    -- CÓDIGO

c1' :: Complejo' Double
c1'=Comp' 3 (-4)

c2' :: Complejo' Float
c2'=3 :::/ 4

c3' :: Complejo' Float
c3'= 1 :::/ 1

c4'::Complejo' Double
c4'=3

c5'::Complejo' Double
c5'=3.2

instance (Fractional n, Eq n) => Eq (Complejo' n) where -- CÓDIGO

instance (Fractional n, Show n, Eq n) => Show (Complejo' n) where -- CÓDIGO

instance (Fractional n) => Num (Complejo' n) where -- CÓDIGO

instance (Fractional n) => Fractional (Complejo' n) where -- CÓDIGO

----------------------------------------

main3 :: IO ()
main3 = do print (c1,c2,c3,c1',c2',c3',c1==c2,c2'==c3',c1*c2==c2*c1)
           print (c1+c2,c2*c1,c1/c1,c1'/c1')
           print (c4,c5)
           print (c4',c5')

{-- RESULTADO DEL PROGRAMA:
(3.0+(-4.0)i,3.0+4.0i,1.0+1.0i,3.0+(-4.0)i,3.0+4.0i,1.0+1.0i,False,False,True)
(6.0+0.0i,25.0+0.0i,1.0+0.0i,1.0+0.0i)
(3.0+0.0i,3.2+0.0i)
(3.0+0.0i,3.2+0.0i)
--}
