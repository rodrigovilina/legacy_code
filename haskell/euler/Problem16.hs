module Problem16 where

import Data.Char ( digitToInt )

string :: [Char]
string = show $ 2 ^ 1000

digits :: [Int]
digits = map digitToInt string

main :: IO ()
main = do
  print $ sum digits
