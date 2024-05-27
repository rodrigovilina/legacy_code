import Shared (askFloat)

extractDecimal :: Float -> Float
extractDecimal n = abs (n - fromIntegral (truncate n))
main = do
  n <- askFloat"Ingrese un número: "
  putStrLn . show . extractDecimal $ n
