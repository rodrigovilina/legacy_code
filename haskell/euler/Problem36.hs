module Problem36 where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

isP :: (Show a, Integral a) => a -> Bool
isP x = isPalindrome x && isPalindromeB x

isPalindromeB :: (Integral a, Show a) => a -> Bool
isPalindromeB z = (==) (showBinary z) (reverse $ showBinary z)

isPalindrome :: Show a => a -> Bool
isPalindrome x = (==) (reverse $ show x) (show x)

showBinary :: (Integral a, Show a) => a -> String
showBinary x = showIntAtBase 2 intToDigit x ""

main = do
  print (sum [x | x <- [1..999999], isP x])
