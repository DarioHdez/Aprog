module Ejercicios2 where

  sumar,sumar',sumar'' :: Integer -> Integer -> Integer
  sumar x y = x + y
  sumar' x = (+) x
  sumar'' = sumar'

  prodEscalar,prodEscalar', prodEscalar'' :: Num a => (a,a) -> (a,a) -> a
  prodEscalar (a,b) (n,m) = (+) (product [a,n]) (product [b,m])
  prodEscalar' (a,b) = prodEscalar (a,b)
  prodEscalar'' = prodEscalar'

  producto,producto',producto'',producto''', producto'''' :: Integer -> Integer -> Integer -> Integer -> Integer
  producto a b c d = product[a,b,c,d]
  producto' a b c = producto a b c
  producto'' a b = producto' a b
  producto''' a = producto'' a
  producto'''' = producto'''

  comprobarSuma = sumar'' x y (x,y <- [1..10] | )
