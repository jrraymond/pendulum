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
                     , _maxVelocity     :: Float
                     , _forceMultiplier :: Float }

makeLenses ''Config

newtype State = State [Float]

getPos (State s) = head s
getVel (State s) = s !! 1
getTh1 (State s) = s !! 2
getAV1 (State s) = s !! 3
getTh2 (State s) = s !! 4
getAV2 (State s) = s !! 5


defaultConfig = Config { _maxTimeSteps    = 10000
                       , _gravity         = -9.8
                       , _cartMass        = 10
                       , _poleLength1     = 1.0
                       , _poleLength2     = 0.5
                       , _poleMass1       = 40.0
                       , _poleMass2       = 40.1
                       , _deltaTime       = 1/60
                       , _angleThreshold  = pi/5.0
                       , _trackLength     = 800
                       , _maxAngVel       = 1.0
                       , _maxVelocity     = 1.0
                       , _forceMultiplier = 100}


initState :: Config -> State
initState c = State [0,0,0.01,0,0.1,0]


step :: Float -> Config -> Float -> State -> State
step dt config action (State st0) = State st1 where
  st1 = rungeKutta (stepFunc config action) dt st0
  stepFunc c a st = let (State st') = stepH c a (State st) in st'
  

stepH :: Config -> Float -> State -> State
stepH config action st = State st' where
  grav = config^.gravity
  force = (action - 0.5) * (config^.forceMultiplier)
  cosTheta1 = cos (getTh1 st)
  sinTheta1 = sin (getTh1 st)
  gcosTheta1 = grav * cosTheta1
  gsinTheta1 = grav * sinTheta1
  cosTheta2 = cos (getTh2 st)
  sinTheta2 = sin (getTh2 st)
  gcosTheta2 = grav * cosTheta2
  gsinTheta2 = grav * sinTheta2
  ml1 = (config^.poleLength1) * (config^.poleMass1)
  ml2 = (config^.poleLength2) * (config^.poleMass2)
  av1 = getAV1 st
  av2 = getAV2 st
  t1 = av1 / ml1
  t2 = av2 / ml2
  fi1 = ml1 * av1 * av1 * sinTheta1 + 0.75 * (config^.poleMass1) * cosTheta1 * (t1 + gsinTheta1)
  fi2 = ml2 * av2 * av2 * sinTheta2 + 0.75 * (config^.poleMass2) * cosTheta2 * (t2 + gsinTheta2)
  mi1 = (config^.poleMass1) * (1 - (0.75 * cosTheta1 * cosTheta1))
  mi2 = (config^.poleMass2) * (1 - (0.75 * cosTheta2 * cosTheta2))
  acc = (force + fi1 + fi2) / (mi1 + mi2 + (config^.cartMass))
  angAcc1 = -0.75 * (acc * cosTheta1 + gsinTheta1 + t1) / (config^.poleLength1)
  angAcc2 = -0.75 * (acc * cosTheta2 + gsinTheta2 + t2) / (config^.poleLength2)
  st' = [getVel st,acc ,av1,angAcc1,av2,angAcc2]
 

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
