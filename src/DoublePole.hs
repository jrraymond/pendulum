{-# LANGUAGE TemplateHaskell #-}
module DoublePole where

import Control.Lens

data Config = Config { _maxTimeSteps    :: Int
                     , _gravity         :: Float
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

makeLenses ''Config

data State = State { _cartPos     :: Float
                   , _cartVel     :: Float
                   , _theta1      :: Float
                   , _angularVel1 :: Float
                   , _theta2      :: Float
                   , _angularVel2 :: Float
                   }

makeLenses ''State

defaultConfig = Config { _maxTimeSteps    = 10000
                       , _gravity         = -9.8
                       , _cartMass        = 10
                       , _poleLength1     = 50
                       , _poleLength2     = 5
                       , _poleMass1       = 4.0
                       , _poleMass2       = 0.1
                       , _deltaTime       = 1/60
                       , _angleThreshold  = pi/5.0
                       , _trackLength     = 800
                       , _maxAngVel       = 1.0
                       , _maxVelocity     = 1.0
                       , _forceMultiplier = 100}


initState :: Config -> State
initState c = State 0 0 0.01 0 (-0.01) 0


step :: Config -> Float -> State -> State
step config action st = st'' where
  st' = rungeKutta (stepH config action) (config^.deltaTime) st
  st'' = rungeKutta (stepH config action) (config^.deltaTime) st'
  

stepH :: Config -> Float -> State -> State
stepH config action st = st' where
  grav = config^.gravity
  force = (action - 0.5) * (config^.forceMultiplier)
  cosTheta1 = cos (st^.theta1)
  sinTheta1 = sin (st^.theta1)
  gcosTheta1 = grav * cosTheta1
  gsinTheta1 = grav * sinTheta1
  cosTheta2 = cos (st^.theta2)
  sinTheta2 = sin (st^.theta2)
  gcosTheta2 = grav * cosTheta2
  gsinTheta2 = grav * sinTheta2
  ml1 = (config^.poleLength1) * (config^.poleMass1)
  ml2 = (config^.poleLength2) * (config^.poleMass2)
  av1 = st^.angularVel1
  av2 = st^.angularVel2
  t1 = av1 / ml1
  t2 = av2 / ml2
  fi1 = ml1 * av1 * av1 * sinTheta1 + 0.75 * (config^.poleMass1) * cosTheta1 * (t1 + gsinTheta1)
  fi2 = ml2 * av2 * av2 * sinTheta2 + 0.75 * (config^.poleMass2) * cosTheta2 * (t2 + gsinTheta2)
  mi1 = (config^.poleMass1) * (1 - (0.75 * cosTheta1 * cosTheta1))
  mi2 = (config^.poleMass2) * (1 - (0.75 * cosTheta2 * cosTheta2))
  acc = (force + fi1 + fi2) / (mi1 + mi2 + (config^.cartMass))
  angAcc1 = -0.75 * (acc * cosTheta1 + gsinTheta1 + t1) / (config^.poleLength1)
  angAcc2 = -0.75 * (acc * cosTheta2 + gsinTheta2 + t2) / (config^.poleLength2)
  st' = State (st^.cartVel) acc
              (st^.angularVel1) angAcc1
              (st^.angularVel2) angAcc2
 
{- classical 4th order runge Kutta for solving differential equations 
-     x_n+1 = x_n + h/6*(k1+2*k2+2*k3+k4) where
- -}
rungeKutta :: (State -> State) -> Float -> State -> State
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
dpToInputs :: Config -> State -> [Float]
dpToInputs config st = [pos,vel,a1,a2,av1,av2] where
  pos = (st^.cartPos) / (config^.trackLength)
  vel = (st^.cartVel) / (config^.maxVelocity)
  a1 = (st^.theta1) / (config^.angleThreshold)
  av1 = (st^.angularVel1) / (config^.maxAngVel)
  a2 = (st^.theta2) / (config^.angleThreshold)
  av2 = (st^.angularVel2) / (config^.maxAngVel)

dpZipWith :: (Float -> Float -> Float) -> State -> State -> State
dpZipWith f st1 st2 = State (f (st1^.cartPos) (st2^.cartPos))
                              (f (st1^.cartVel) (st2^.cartVel))
                              (f (st1^.theta1) (st2^.theta1))
                              (f (st1^.angularVel1) (st2^.angularVel1))
                              (f (st1^.theta2) (st2^.theta2))
                              (f (st1^.angularVel2) (st2^.angularVel2))


dpMap :: (Float -> Float) -> State -> State
dpMap f = over cartPos f .
          over cartVel f .
          over theta1 f .
          over angularVel1 f .
          over theta2 f .
          over angularVel2 f


dpToList :: State -> [Float]
dpToList st = [st^.cartPos, st^.cartVel
              ,st^.theta1,st^.angularVel1
              ,st^.theta2,st^.angularVel2]

run :: ([Float] -> Float) -> Config -> State -> Float
run f config st = result where
  inputs = dpToInputs config st
  output = f inputs
  result = undefined
