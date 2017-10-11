module Replicate where
  import GHC.List


  replicate' :: (Int,a) -> [a]
  replicate' (1,x) = [x]
  replicate' (n,x) = [x] ++ replicate' ((n-1),x)

  {--
   La funcion curry cambia el modo en el que una funcion curryficada recibe
   los argumentos. Poner fotos. En nuestro caso hemos cambiado el modo de
   recepcion de replicate, al igual que hace curry.
--}
