module Problem15 where

latticeLength :: Int -> Int -> Int
latticeLength 0 0 = 1
latticeLength x 0 = latticeLength (x-1) 0
latticeLength 0 y = latticeLength (y-1) 0
latticeLength x y = if x > y then
    latticeLength (x-1) y + latticeLength x (y-1)
  else
    latticeLength (y-1) x + latticeLength y (x-1)

main :: IO ()
main = do
  print $ latticeLength 15 15
