extractDigit n pos = (n `div` 10 ^ pos) `mod` 10
isPerfectCube x = ((extractDigit x 0) ^ 3) + ((extractDigit x 1) ^ 3) + ((extractDigit x 2) ^ 3 )== x
asdf = [x | x <- [100..999], isPerfectCube x]

main = do
  putStrLn . show $ asdf

