import Shared (askInt)

main = do
  actual <- askInt "Hora actual: "
  offset <- askInt "Cantidad de horas: "
  putStrLn ("En " ++ show offset ++ " horas, el reloj marcará las " ++ show ((actual + offset) `mod` 12))


