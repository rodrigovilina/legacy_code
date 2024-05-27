primeFactors :: Int -> [Int]
primeFactors n =
  case smallerFactor of
    [] -> [n]
    _  -> (++) smallerFactor (primeFactors (div n (head smallerFactor)))
  where
    smallerFactor = take 1 (filter (\x -> mod n x == 0) [2..n-1])

main :: IO ()
main = print (last (primeFactors 600851475143))
