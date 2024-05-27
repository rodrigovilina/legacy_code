import Data.List ( intersect, nub )

isPalindrome :: Int -> Bool
isPalindrome n = show n == reverse (show n)

range :: [Int]
range = [100..999]

intLesserThanOneMill :: [Int]
intLesserThanOneMill = [1 .. (999 * 999)]

palindromes :: [Int]
palindromes = filter isPalindrome intLesserThanOneMill

timesNumbersBetween :: Int -> [Int]
timesNumbersBetween x =  map (x *) range

threeByThree :: [Int]
threeByThree = nub (concatMap timesNumbersBetween range)
-- threeByThree = nub [ x * y | x <- [100..999], y <- [100..999] ]

biggestPalindrome :: Int
biggestPalindrome = maximum (palindromes `intersect` threeByThree)

main :: IO ()
main = print biggestPalindrome
