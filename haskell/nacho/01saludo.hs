main = do
  putStr "Ingrese su nombre: "
  name <- getLine
  putStrLn ("Hola, " ++ name)
