module Print2 where

mygreet :: String
mygreet = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
  putStrLn mygreet
  putStrLn dosgreet
  where dosgreet =
          concat [hello, " ", world]
