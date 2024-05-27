import Data.List
import Data.Numbers.Primes

indexForPrimeAbove :: Int -> Maybe Int
indexForPrimeAbove number = find (> number) primes

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst cond (lhead:ltail) | cond lhead = lhead : filterFirst cond ltail
                               | otherwise = []

primesLesserThan :: Int -> [Int]
primesLesserThan number = filterFirst (< number) primes

main :: IO ()
main = print (sum $ primesLesserThan 2000000)
