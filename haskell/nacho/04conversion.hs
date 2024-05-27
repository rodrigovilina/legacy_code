import Shared (askFloat)

cmToIn x = x / 2.54

main = do
  l <- askFloat "Ingrese longitud: "
  putStrLn (show l ++ " cm = " ++ show (cmToIn l) ++ " in")
