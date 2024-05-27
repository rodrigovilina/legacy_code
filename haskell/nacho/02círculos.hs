area r = pi * r ** 2
perimeter r = 2 * pi * r

main = do
  putStr "Ingerse el radio: "
  r <- readLn :: IO Float
  putStrLn ("Perímetro: " ++ show (perimeter r))
  putStrLn ("Área: " ++ show (area r))
