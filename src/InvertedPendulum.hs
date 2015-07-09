{-# LANGUAGE TemplateHaskell #-}
module InvertedPendulum where

import RungeKutta

import Control.Lens


data Config = Config { _maxTimeSteps    :: Float
                     , _cartMass        :: Float
                     , _pendulumMass    :: Float
                     , _pendulumLength  :: Float
                     , _trackLength     :: Float 
                     , _deltaTime       :: Float
                     , _gravity         :: Float
                     , _maxAngVelocity  :: Float
                     , _maxVelocity     :: Float
                     , _forceMultiplier :: Float }

makeLenses ''Config

newtype State = State [Float]

getCartPos :: State -> Float
getCartPos (State xs) = head xs

getCartVel :: State -> Float
getCartVel (State xs) = xs !! 1

getTheta :: State -> Float
getTheta (State xs) = xs !! 2

getAngVel :: State -> Float
getAngVel (State xs) = xs !! 3



defaultConfig :: Config
defaultConfig = Config { _maxTimeSteps    = 10000
                       , _cartMass        = 10.0
                       , _pendulumMass    = 10.0
                       , _pendulumLength  = 1.0
                       , _trackLength     = 800
                       , _deltaTime       = 1/60
                       , _gravity         = 9.8
                       , _maxAngVelocity  = 1.0
                       , _maxVelocity     = 1.0
                       , _forceMultiplier = 4 }


initState :: Config -> State
initState c = State [ 0, 0, 0.10, 0 ]


step :: Float -> Config -> Float -> State -> State
step dt c action (State st0) = State st1 where
  st1 = rungeKutta (stepFunc c action) dt st0
  stepFunc :: Config -> Float -> [Float] -> [Float]
  stepFunc c a st = let (State st') = stepH c a (State st) in st'


stepH :: Config -> Float -> State -> State
stepH c action st = State st' where
  f = (action - 0.5) * (c^.forceMultiplier)
  mp = c^.pendulumMass
  mc = c^.cartMass
  l = c^.pendulumLength
  g = c^.gravity
  theta = getTheta st
  av = getAngVel st
  cosTh = cos theta
  sinTh = sin theta
  d1 = mc + mp - mp * cosTh
  d2 = l * (mc + mp - mp * cosTh ^ 2)
  ac = (f + l * mp * av ^ 2 * sinTh - mp * g * sinTh * cosTh) / d1
  aac = (f * cosTh + (mc + mp) * g * sinTh - l * mp * av ^ 2  * sinTh * cosTh) / d2
  st' = [getCartVel st, ac, getAngVel st, aac]
