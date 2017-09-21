module Ejercicios where

  x :: String
  x = "Cadena"

  y :: Int
  y = length x + 7

  z :: Integer
  z = 20

  a :: Num a => a
  a = 34

  suma1 :: Integer -> Integer -> Integer
  suma1 a b = a + b

  suma2 :: Num a => a -> a -> a
  suma2 a b = a + b

  main :: IO()
  main = do { print x;
            print y;
            print a;
            print (z,a);
            print [z,a];
            print (suma1 2 3);
            print (suma2 23 10); }
