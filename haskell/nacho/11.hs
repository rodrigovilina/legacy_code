import Shared (askInt)

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

parity n = if' (mod n 2 == 0) "Su número es par" "Su número es impar"

main = do
  n <- askInt "Ingrese un número: "
  putStrLn (parity n)
