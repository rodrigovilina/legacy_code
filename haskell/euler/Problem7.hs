{-# LANGUAGE NumericUnderscores #-}

nthPrime :: Int -> Int
nthPrime n = primes !! (n-1)

primes :: [Int]
primes = filterPrime [2..]
  where filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]

main :: IO ()
main = print(nthPrime 10_001)
