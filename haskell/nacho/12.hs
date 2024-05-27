import Shared (askInt)

a = "Ingrese un anno: "
b = "es bisiesto"
c = "no es bisiesto"

bisiesto n
  | n < 1582  = (mod n 4 == 0) 
  | otherwise = (mod n 4 == 0) && (mod n 100 /= 0)|| (mod n 400 == 0)
            
 
main = do
  n <- askInt(a)
  putStrLn(if bisiesto(n) then b else c) 
