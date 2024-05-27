import Shared (askWord)

diff p1 p2 = length (p1) - length (p2)

phrase p1 p2  = ("La palabra " ++ p1 ++ " tiene " ++ show (diff p1 p2) ++" letras más que " ++ p2)

result p1 p2
    | (diff p1 p2) > 0 = phrase p1 p2 
    | (diff p1 p2) < 0 = phrase p2 p1 
    | otherwise        = ("La palabra " ++ p1 ++ " tiene el mismo largo que la palabra " ++ p2)

main = do
  p1 <- askWord "Palabra 1: "
  p2 <- askWord "Palabra 2: "
  putStrLn (result p1 p2)
