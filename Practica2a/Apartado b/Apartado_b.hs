module Tautologias where
 --import Data.List.Split
-- Tautology checker --
-----------------

 data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop | Or Prop Prop | DImply Prop Prop  deriving Show

 type Subst =  Assoc Char Bool

 type Assoc k v =  [(k,v)]

 p1 :: Prop
 p1 = And (Var 'A') (Not (Var 'A'))

 p2 :: Prop
 p2 = Imply  (And (Var 'A') (Var 'B')) (Var 'A')

 (¬) :: Prop -> Prop
 (¬) = Not

 (-->) :: Prop -> Prop -> Prop
 (-->) = Imply

 (<-->) :: Prop -> Prop -> Prop
 --(<-->) a b = And (Imply a b) (Imply b a)
 (<-->) = DImply

 (/\) :: Prop -> Prop -> Prop
 (/\)  = And

 (\/) :: Prop -> Prop -> Prop
 --(\/) a b = And (Not a) (Not b)
 (\/) = Or


 find :: Eq k => k -> Assoc k v -> v
 find k t =  head [v | (k',v) <- t, k == k']

 eval :: Subst -> Prop -> Bool
 eval _ (Const b)  =  b
 eval s (Var x)  =  find x s
 eval s (Not p) = not (eval s p)
 eval s (And p q) =  eval s p && eval s q
 eval s (Imply p q) =  eval s p <= eval s q
 eval s (Or p q) = eval s p || eval s q
 eval s (DImply p q) = eval s (Imply p q) && eval s (Imply q p)

 vars :: Prop -> [Char]
 vars (Const _) =  []
 vars (Var x) = [x]
 vars (Not p) =  vars p
 vars (And p q) =  vars p ++ vars q
 vars (Imply p q) =  vars p ++ vars q
 vars (Or p q) = vars p ++ vars q
 vars (DImply p q) = vars p ++ vars q

 bools :: Int -> [[Bool]]
 bools 0 =  [[]]
 bools n =  map (False:) bss ++ map (True:) bss
              where bss = bools (n-1)

 rmdups :: Eq a => [a] -> [a]
 rmdups [] =  []
 rmdups (x:xs) =  x : rmdups (filter (/= x) xs)

 substs :: Prop -> [Subst]
 substs p =  map (zip vs) (bools (length vs))
              where vs = rmdups (vars p)

 isTaut :: Prop -> Bool
 isTaut p =  and [eval s p | s <- substs p]

 toString :: Prop -> String
 toString (Const x) = show x
 toString (Var x) = [x]
 toString (Not x) =  "(¬" ++ toString x ++ ")"
 toString (And x y) =  "(" ++ toString x ++ "/\\" ++ toString y ++ ")"
 toString (Or x y) =  "(" ++ toString x ++ "\\/" ++ toString y ++ ")"
 toString (Imply x y) = "(" ++ toString x ++ "-->" ++ toString y ++ ")"
 toString (DImply x y) = "(" ++ toString x ++ "<-->" ++ toString y ++ ")"

 myPrint :: Prop -> IO()
 myPrint x = putStrLn $ toString x

 -- Funcion principal que pasa un string en notacion posfija a proposicion
 -- Devuelve Nothing si la cadena de entrada esta mal, en caso contrario da Just (proposicion correcta)
 posfixToInfix :: String -> Maybe Prop
 posfixToInfix s = if (length (vars (polacaInversa s)) /= checkPolaca s) then Nothing
                    else Just (polacaInversa s)


 -- Funcion auxiliar que pasa un string en notacion posfija a una preposicion.
 -- Va guardando en una lista las proposiciones, y se queda con la ultima que genera.
 polacaInversa :: String -> Prop
 polacaInversa = head . foldl funcionPliegue [] . words
                    where   funcionPliegue (y:ys) "¬"      = Not y:ys
                            funcionPliegue (x:y:ys) "/\\"  = And y x:ys
                            funcionPliegue (x:y:ys) "\\/"  = Or y x:ys
                            funcionPliegue (x:y:ys) "-->"  = Imply y x:ys
                            funcionPliegue (x:y:ys) "<-->" = DImply y x:ys
                            funcionPliegue xs cadena     = Var (head cadena) : xs

 -- Funcion auxiliar que calcula el numero de variables que tendrian que quedar en la proposicion final
 -- Recibe la misma cadena que la funcion principal.
 checkPolaca :: String -> Int
 checkPolaca s = length a where
                    a = [l | l <- words s, l /= "¬", l/= "/\\", l /= "\\/", l /= "-->", l /= "<-->"]
