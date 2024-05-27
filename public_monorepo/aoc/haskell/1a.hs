input :: [Integer]
input = [199,200,208,210,200,207,240,269,260,263]

mapLines :: (String -> b) -> String -> [b]
mapLines func string = map func (lines string)

eachCons :: [a] -> [(a, a)]
eachCons = zip<*>tail

countFilter :: (a -> Bool) -> [a] -> Int
countFilter pred = length . filter pred

output :: Ord a => [a] -> Int
output numbers = countFilter id [j < k | (j, k) <- eachCons numbers]

main :: IO ()
main = do
  print (output input)
  file_input <- readFile "../inputs/input1.txt"
  let numbers = mapLines read file_input :: [Integer]
  print (output numbers)
