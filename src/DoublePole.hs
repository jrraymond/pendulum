{-# LANGUAGE TemplateHaskell #-}
module DoublePole where

import Control.Lens

data DPConfig = DPConfig { _maxTimeSteps    :: Int
                         , _gravity         :: Float
                         , _frictionCoeff   :: Float
                         , _cartMass        :: Float
                         , _poleLength1     :: Float
                         , _poleLength2     :: Float
                         , _poleMass1       :: Float
                         , _poleMass2       :: Float
                         , _deltaTime       :: Float
                         , _angleThreshold  :: Float
                         , _trackLength     :: Float
                         , _maxAngVel       :: Float
                         , _maxVelocity     :: Float
                         , _forceMultiplier :: Float }

makeLenses ''DPConfig

data DPstate = DPstate { _cartPos     :: Float
                       , _cartVel     :: Float
                       , _theta1      :: Float
                       , _angularVel1 :: Float
                       , _theta2      :: Float
                       , _angularVel2 :: Float
                       }

makeLenses ''DPstate

defaultDPConfig = DPConfig { _maxTimeSteps    = 10000
                           , _gravity         = -9.80
                           , _frictionCoeff   = 0.1
                           , _cartMass        = 10
                           , _poleLength1     = 2.0
                           , _poleLength2     = 1.0
                           , _poleMass1       = 0.4
                           , _poleMass2       = 0.2
                           , _deltaTime       = 1 / 60
                           , _angleThreshold  = pi / 5.0
                           , _trackLength     = 800
                           , _maxAngVel       = 1.0
                           , _maxVelocity     = 1.0
                           , _forceMultiplier = 100}


initDP :: DPConfig -> DPstate
initDP c = DPstate 0 0 0.01 0 (-0.01) 0


step :: DPConfig -> Float -> DPstate -> DPstate
step config action dp = dp'' where
  dp' = rungeKutta (stepFunc config action) (config^.deltaTime) dp
  dp'' = rungeKutta (stepFunc config action) (config^.deltaTime) dp'
  

stepFunc :: DPConfig -> Float -> DPstate -> DPstate
stepFunc config action dp = dp'' where
  (ac,aac1,aac2) = stepH config action dp
  dp'' = DPstate (dp^.cartVel) ac
                 (dp^.angularVel1) aac1
                 (dp^.angularVel2) aac2


stepH :: DPConfig -> Float -> DPstate -> (Float,Float,Float)
stepH config action dp = (acc,angAcc1,angAcc2) where
  grav = config^.gravity
  force = (action - 0.5) * (config^.forceMultiplier)
  cosTheta1 = cos (dp^.theta1)
  sinTheta1 = sin (dp^.theta1)
  gcosTheta1 = grav * cosTheta1
  gsinTheta1 = grav * sinTheta1
  cosTheta2 = cos (dp^.theta2)
  sinTheta2 = sin (dp^.theta2)
  gcosTheta2 = grav * cosTheta2
  gsinTheta2 = grav * sinTheta2
  ml1 = (config^.poleLength1) * (config^.poleMass1)
  ml2 = (config^.poleLength2) * (config^.poleMass2)
  av1 = dp^.angularVel1
  av2 = dp^.angularVel2
  t1 = 0.000002 * av1 / ml1
  t2 = 0.000002 * av2 / ml2
  fi1 = ml1 * av1 * av1 * sinTheta1 + 0.75 * (config^.poleMass1) * cosTheta1 * (t1 + gsinTheta1)
  fi2 = ml2 * av2 * av2 * sinTheta2 + 0.75 * (config^.poleMass2) * cosTheta2 * (t2 + gsinTheta2)
  mi1 = (config^.poleMass1) * (1 - (0.75 * cosTheta1 * cosTheta1))
  mi2 = (config^.poleMass2) * (1 - (0.75 * cosTheta2 * cosTheta2))
  acc = (force + fi1 + fi2) / (mi1 + mi2 + (config^.cartMass))
  angAcc1 = -0.75 * (acc * cosTheta1 + gsinTheta1 + t1) / (config^.poleLength1)
  angAcc2 = -0.75 * (acc * cosTheta2 + gsinTheta2 + t2) / (config^.poleLength2)
 
{- classical 4th order Runge Kutta for solving differential equations 
-     x_n+1 = x_n + h/6*(k1+2*k2+2*k3+k4) where
- -}
rungeKutta :: (DPstate -> DPstate) -> Float -> DPstate -> DPstate
rungeKutta stepF h xn = xn' where
  k1s = stepF xn
  k2s = stepF $ dpZipWith (\a b -> a + h / 2 * b) xn k1s
  k3s = stepF $ dpZipWith (\a b -> a + h / 2 * b) xn k2s
  k4s = stepF $ dpZipWith (\a b -> a + h * b) xn k3s
  k2s' = dpMap (*2) k2s
  k3s' = dpMap (*2) k3s
  ks = dpZipWith (+) k4s $ dpZipWith (+) k3s' $ dpZipWith (+) k2s' k1s
  xn' = dpZipWith (\a b -> a + h / 6 * b) xn ks



--TODO: normalize inputs to 0-1
dpToInputs :: DPConfig -> DPstate -> [Float]
dpToInputs config dp = [pos,vel,a1,a2,av1,av2] where
  pos = (dp^.cartPos) / (config^.trackLength)
  vel = (dp^.cartVel) / (config^.maxVelocity)
  a1 = (dp^.theta1) / (config^.angleThreshold)
  av1 = (dp^.angularVel1) / (config^.maxAngVel)
  a2 = (dp^.theta2) / (config^.angleThreshold)
  av2 = (dp^.angularVel2) / (config^.maxAngVel)

dpZipWith :: (Float -> Float -> Float) -> DPstate -> DPstate -> DPstate
dpZipWith f dp1 dp2 = DPstate (f (dp1^.cartPos) (dp2^.cartPos))
                              (f (dp1^.cartVel) (dp2^.cartVel))
                              (f (dp1^.theta1) (dp2^.theta1))
                              (f (dp1^.angularVel1) (dp2^.angularVel1))
                              (f (dp1^.theta2) (dp2^.theta2))
                              (f (dp1^.angularVel2) (dp2^.angularVel2))


dpMap :: (Float -> Float) -> DPstate -> DPstate
dpMap f = over cartPos f .
          over cartVel f .
          over theta1 f .
          over angularVel1 f .
          over theta2 f .
          over angularVel2 f


dpToList :: DPstate -> [Float]
dpToList dp = [dp^.cartPos, dp^.cartVel
              ,dp^.theta1,dp^.angularVel1
              ,dp^.theta2,dp^.angularVel2]

run :: ([Float] -> Float) -> DPConfig -> DPstate -> Float
run f config dp = result where
  inputs = dpToInputs config dp
  output = f inputs
  result = undefined
