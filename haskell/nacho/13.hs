import Shared (askInt)

a ="Dividendo: "
b = "Divisor: "

c = "La división no es exacta"
d = "La división es exacta"
e = "Cociente: "
f = "Resto: "

main = do
  n <- askInt a
  m <- askInt b

  putStrLn (if mod n m == 0 then d else c)
  putStrLn (e ++ show (div n m))
  putStrLn (f ++ show (mod n m))
