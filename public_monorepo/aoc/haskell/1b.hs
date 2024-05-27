chop :: ([a] -> (b, [a])) -> [a] -> [b]
chop _ [] = []
chop f as = b : chop f as'
  where (b, as') = f as

divvy :: Int -> Int -> [a] -> [[a]]
divvy _ _ [] = []
divvy n m lst = filter (\ws -> n == length ws) choppedl
  where choppedl = chop (\xs -> (take n xs , drop m xs)) lst

input :: [Integer]
input = [199,200,208,210,200,207,240,269,260,263]

threeItemWindows :: [a] -> [[a]]
threeItemWindows = divvy 3 1

threeItemReadings :: Num b => [b] -> [b]
threeItemReadings input = map sum (threeItemWindows input)

consecutiveThreeItemReadings :: Num a => [a] -> [[a]]
consecutiveThreeItemReadings input = divvy 2 1 (threeItemReadings input)

compare :: Ord a => [a] -> Bool
compare (x:y:_) = x < y
compare [] = False
compare [_] = False

output :: (Ord a, Num a) => [a] -> [Bool]
output input = map Main.compare (consecutiveThreeItemReadings input)

solution :: (Ord a, Num a) => [a] -> Int
solution input = length (filter id (output input))

mapLines :: (String -> b) -> String -> [b]
mapLines func string = map func (lines string)

main :: IO ()
main = do
  print $ solution input
  file_input <- readFile "../inputs/input1.txt"
  let numbers = mapLines read file_input
  print (solution numbers)
