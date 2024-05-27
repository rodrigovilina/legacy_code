import Shared (askInt)

hypotenuse :: Int -> Int -> Float
hypotenuse a b = sqrt (fromIntegral (a ^ 2 + b ^ 2))

main = do
  a <- askInt "Ingrese cateto a: "
  b <- askInt "Ingrese cateto b: "
  putStrLn ("La hipotenusa es " ++ show (hypotenuse a b))
