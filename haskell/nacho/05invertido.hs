import Shared (askInt)

extractDigit :: Int -> Int -> Int
extractDigit n pos = (n `div` 10 ^ pos) `mod` 10

invertThreeDigitsNumber :: Int -> Int
invertThreeDigitsNumber x = (extractDigit x 0) * 100 + (extractDigit x 1) * 10 + (extractDigit x 2)

main = do
  x <- askInt "Ingrese Número: "
  putStrLn (show (invertThreeDigitsNumber x))
