{-# LANGUAGE TemplateHaskell #-}
module SinglePole where

import RungeKutta

import Control.Lens


data Config = Config { _maxTimeSteps    :: Float
                     , _cartMass        :: Float
                     , _poleMass        :: Float
                     , _poleLength      :: Float
                     , _trackLength     :: Float 
                     , _deltaTime       :: Float
                     , _gravity         :: Float
                     , _maxAngVelocity  :: Float
                     , _maxVelocity     :: Float}

makeLenses ''Config

newtype State = State [Float]

getPos (State xs) = head xs
getVel (State xs) = xs !! 1
getTh (State xs) = xs !! 2
getAV (State xs) = xs !! 3



defaultConfig :: Config
defaultConfig = Config { _maxTimeSteps    = 10000
                       , _cartMass        = 10.0
                       , _poleMass    = 10.0
                       , _poleLength  = 1.0
                       , _trackLength     = 800
                       , _deltaTime       = 1/60
                       , _gravity         = 9.8
                       , _maxAngVelocity  = 1.0
                       , _maxVelocity     = 1.0}


initState :: Config -> State
initState c = State [0,0,0.1,0]


step :: Float -> Config -> Float -> State -> State
step dt c action (State st0) = State st1 where
  st1 = rungeKutta (stepFunc c action) dt st0
  stepFunc :: Config -> Float -> [Float] -> [Float]
  stepFunc c a st = let (State st') = stepH c a (State st) in st'

 
stepH :: Config -> Float -> State -> State
stepH c action st = State st' where
  f = (action - 0.5) * (mc + mp)
  mp = c^.poleMass
  mc = c^.cartMass
  l = c^.poleLength
  g = c^.gravity
  theta = getTh st
  av = getAV st
  cosTh = cos theta
  sinTh = sin theta
  d1 = mc + mp - mp * cosTh
  d2 = l * (mc + mp - mp * cosTh ^ 2)
  ac = (f + l * mp * av ^ 2 * sinTh - mp * g * sinTh * cosTh) / d1
  aac = (f * cosTh + (mc + mp) * g * sinTh - l * mp * av ^ 2  * sinTh * cosTh) / d2
  st' = [getVel st, ac, getAV st, aac]
