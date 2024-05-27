sumOfSquares :: (Num a, Enum a) => a -> a
sumOfSquares x = sum (map (^ 2) [1..x])

squareOfSums :: (Num a, Enum a) => a -> a
squareOfSums x = sum [1..x] ^ 2

main = print(sumOfSquares 100 - squareOfSums 100)
