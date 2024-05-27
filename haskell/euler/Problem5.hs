import Data.List ( find, nub )

asdf :: Integral a => a -> [Bool]
asdf x = map (\y -> mod x y == 0) [1..20]

assdf :: Integral a => a -> Bool
assdf x = nub (asdf x) == [True]

findIt :: Maybe Integer
findIt = find assdf [1..]

main :: IO ()
main = print findIt
