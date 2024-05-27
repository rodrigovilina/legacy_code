import Data.List

factorsOf :: Integer-> [Integer]
factorsOf n = [ x | x <- [1..n], mod n x == 0]

-- https://rosettacode.org/wiki/Factors_of_an_integer#Haskell
integerFactors :: Int -> [Int]
integerFactors n
  | 1 > n = []
  | otherwise = lows ++ fmap (quot n) (part n (reverse lows))
  where
    part n
      | n == square = tail
      | otherwise = id
    (square, lows) =
      (,) . (^ 2) <*> (filter ((0 ==) . rem n) . enumFromTo 1) $
      floor (sqrt $ fromIntegral n)

main :: IO ()
main = do
  print $ find (\x -> length (integerFactors x) > 500) [ sum [1..x] | x <- [1..]]
