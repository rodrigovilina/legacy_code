import Shared (askFloat)

rho = 1.038
c = 3.7
k = 5.4e-3
tw = 100
ty = 70

numerator m = ((m  ** (2 / 3))* c * (rho ** (1 / 3)))
denominator = k * (pi ** 2) * (4 * pi / 3) ** (2 / 3) 
log' t0 = log (0.76 * (t0 - tw) / (ty - tw) ) 
t m t0 = (numerator m) / denominator * (log' t0)

main = do
  m <- askFloat "Pasa la masa: "
  t0 <- askFloat "Pasa la temperatura: "
  putStrLn (show  (t m t0))
