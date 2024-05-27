import Shared (askInt)

calcC3 :: Int -> Int -> Int -> Int
calcC3 c1 c2 lab = max (truncate (((180 - 0.9 * fromIntegral lab) / 0.7) - fromIntegral c1 - fromIntegral c2)) 0

main = do
  c1 <- askInt "Ingrese nota certamen 1: "
  c2 <- askInt "Ingrese nota certamen 2: "
  lab <- askInt "Ingrese nota laboratorio "
  putStrLn ("Necesita nota " ++ show (calcC3 c1 c2 lab) ++ " en el certamen 3")
