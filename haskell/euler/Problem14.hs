module Problem14 where

import Data.List

collatz :: Integral a => a -> [a]
collatz n | (==) n 1 = [1]
          | even n = n : collatz (quot n 2)
          | odd n = n : collatz (3*n + 1)

allCollatz :: [[Int]]
allCollatz = map (\x -> [length $ collatz x, x]) [1..1000000]

maxPair :: [Int]
maxPair = maximumBy (\x y -> compare (head x) (head y)) allCollatz

main :: IO ()
main = do
  print $ last maxPair
