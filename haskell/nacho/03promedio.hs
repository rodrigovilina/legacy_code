import Shared (askFloat)

average a b c d = (a + b + c + d) / 4.0

main = do
  a <- askFloat "Primera nota: "
  b <- askFloat "Segunda nota: "
  c <- askFloat "Tercera nota: "
  d <- askFloat "Cuarta nota: "
  putStrLn ("El promedio es: " ++ show (average a b c d))
