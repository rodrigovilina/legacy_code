fibs :: [Int]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)
fib :: Int -> Int
fib n = fibs !! n

accumulate :: Int-> Int -> Int-> Int
accumulate acc index limit | fib index > limit = acc
                           | even (fib index) = accumulate ((+) acc (fib index)) ((+ )index 1) limit
                           | otherwise = accumulate acc ((+) index 1) limit

sumFibUpTo :: Int-> Int
sumFibUpTo = accumulate 0 1

main :: IO ()
main = print (sumFibUpTo 4000000)
