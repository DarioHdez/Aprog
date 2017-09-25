module Ejercicios where
 {-# LANGUAGE Rank2Types #-}

  x :: String
  x = "Cadena"

  y :: Int
  y = length x + 7

  z :: Integer
  z = 20

  a :: Num a => a
  a = 34

 -- Tupla de numero polimorfico
  nums1 :: Num a => (Integer, a)
  nums1 = (z,a)

  nums2 :: [Integer]
  nums2 = [z,a]

  suma1 :: Integer -> Integer -> Integer
  suma1 a b = a + b

  suma2 :: Num a => a -> a -> a
  suma2 a b = a + b

  --funcionTupla = (suma1 2 3, suma2 4 5)
  funcionTupla :: Num a => (Integer -> Integer -> Integer, a -> a -> a)
  funcionTupla = (suma1, suma2)

  listaTupla :: [Integer -> Integer -> Integer]
  listaTupla = [suma1, suma2]

  type ListaEnteros = [Integer]
  listaPrueba :: ListaEnteros
  listaPrueba = [5,5,4,7,6]

  data Numero = Entero Integer | Decimal Double
  numEjemplo :: Numero
  numEjemplo = Entero 3

  type FuncionEntreEnteros = Integer -> Integer
  suma1Parc :: FuncionEntreEnteros
  suma1Parc  = suma1 2

  prueba = suma1Parc 3

  {--data Funcion a b where
    F :: (Num a, Num b) => (a -> b) -> Funcion a b

  f2 :: Funcion Integer Integer
  f2 = F (suma1 8) --}

  type Funcion' a b = (Num a, Num b) => a->b

  f2' :: Funcion' Integer Integer
  f2' = suma1 8
