module RungeKutta where

{- classical 4th order Runge Kutta for solving differential equations 
-     x_n+1 = x_n + h/6*(k1+2*k2+2*k3+k4) where
- -}
rungeKutta :: ([Float] -> [Float]) -> Float -> [Float] -> [Float]
rungeKutta stepF h xn = xn' where
  k1s = stepF xn
  k2s = stepF $ zipWith (\a b -> a + h / 2 * b) xn k1s
  k3s = stepF $ zipWith (\a b -> a + h / 2 * b) xn k2s
  k4s = stepF $ zipWith (\a b -> a + h * b) xn k3s
  k2s' = map (*2) k2s
  k3s' = map (*2) k3s
  ks = zipWith (+) k4s $ zipWith (+) k3s' $ zipWith (+) k2s' k1s
  xn' = zipWith (\a b -> a + h / 6 * b) xn ks
