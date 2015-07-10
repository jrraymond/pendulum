{-# LANGUAGE TemplateHaskell #-}
module DoublePole where

import RungeKutta

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
                     , _maxVelocity     :: Float}

makeLenses ''Config

newtype State = State [Float]

getPos (State s) = head s
getVel (State s) = s !! 1
getTh1 (State s) = s !! 2
getAV1 (State s) = s !! 3
getTh2 (State s) = s !! 4
getAV2 (State s) = s !! 5


defaultConfig = Config { _maxTimeSteps    = 10000
                       , _gravity         = 9.8
                       , _cartMass        = 100
                       , _poleLength1     = 1.0
                       , _poleLength2     = 0.5
                       , _poleMass1       = 4.0
                       , _poleMass2       = 8.0
                       , _deltaTime       = 1/60
                       , _angleThreshold  = pi/5.0
                       , _trackLength     = 800
                       , _maxAngVel       = 1.0
                       , _maxVelocity     = 1.0}


initState :: Config -> State
initState c = State [0,0,0.01,0,0,0]


step :: Float -> Config -> Float -> State -> State
step dt config action (State st0) = State st1 where
  st1 = rungeKutta (stepFunc config action) dt st0
  stepFunc c a st = let (State st') = stepH c a (State st) in st'
  

stepH :: Config -> Float -> State -> State
stepH config action st = State st' where
  g = config^.gravity
  force = (action - 0.5) * (config^.cartMass + m1 + m2)
  cosA1 = cos (getTh1 st)
  sinA1 = sin (getTh1 st)
  cosA2 = cos (getTh2 st)
  sinA2 = sin (getTh2 st)
  av1 = getAV1 st
  av2 = getAV2 st
  m1 = config^.poleMass1
  m2 = config^.poleMass2
  l1 = config^.poleLength1
  l2 = config^.poleLength2
  t1 = m1 * (g * sinA1 * cosA1 - l1 * av1 ^ 2 * sinA1)
  t2 = m2 * (g * sinA2 * cosA2 - l2 * av2 ^ 2 * sinA2)
  d1 = m1 + m2 + (config^.cartMass) - m1 * cosA1 ^ 2 - m2 * cosA2 ^ 2
  ac = (force - t1 - t2) / d1
  aac1 = (g * sinA1 - ac * cosA1) / l1
  aac2 = (g * sinA2 - ac * cosA2) / l2
  st' = [getVel st,ac,getAV1 st,aac1,getAV2 st,aac2]


--TODO: normalize inputs to 0-1
dpToInputs :: Config -> State -> [Float]
dpToInputs config st = [pos,vel,a1,a2,av1,av2] where
  pos = getPos st / (config^.trackLength)
  vel = getVel st / (config^.maxVelocity)
  a1 = getTh1 st / (config^.angleThreshold)
  av1 = getAV1 st / (config^.maxAngVel)
  a2 = getTh2 st / (config^.angleThreshold)
  av2 = getAV2 st / (config^.maxAngVel)


run :: ([Float] -> Float) -> Config -> State -> Float
run f config st = result where
  inputs = dpToInputs config st
  output = f inputs
  result = undefined
