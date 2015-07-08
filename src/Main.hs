{-# LANGUAGE PatternGuards #-}

import DoublePole

import Control.Lens
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as G
import Numeric

type World = (Float,DPConfig, DPstate)

windowW = 800
windowH = 400

drawDP :: World -> Picture
drawDP (_,c,dp) = Pictures [equator,cart,pole1,ball1,pole2,ball2,info] where
  cposx = dp^.cartPos
  scale = 100
  scaleM = 10
  cartW = scaleM * (c^.cartMass)
  cartH = scaleM / 2 * (c^.cartMass)
  b1x = cposx + scale * (c^.poleLength1) * sin (dp^.theta1)
  b1y = scale * (c^.poleLength1) * cos (dp^.theta1)
  b2x = cposx + scale * (c^.poleLength2) * sin (dp^.theta2)
  b2y = scale * (c^.poleLength2) * cos (dp^.theta2)
  cart = translate cposx 0 (rectangleWire cartW cartH)
  pole1 = line [(cposx,0),(b1x,b1y)]
  ball1 = translate b1x b1y $ Scale scaleM scaleM $ Circle (10 * (c^.poleMass1))
  pole2 = line [(cposx,0),(b2x,b2y)]
  ball2 = translate b2x b2y $ Scale scaleM scaleM $ Circle (10 * (c^.poleMass2))
  equator = line [(negate windowW,0),(windowW,0)]
  info = Translate (windowW/(-2) + 10) (windowH/(-2) + 10) $ Scale 0.1 0.1 $ 
          Text $ "cart_x: " ++ fmtFloat cposx 4 ++ " | theta 1: " ++ 
                fmtFloat (dp^.theta1) 4 ++ " | theta2: " ++ fmtFloat (dp^.theta2) 4


handleInput :: G.Event -> World -> World
handleInput (G.EventKey key keystate _ _) (_,c,dp) 
  | G.SpecialKey G.KeyLeft  <- key, G.Down <- keystate = (0,c,dp)
  | G.SpecialKey G.KeyRight <- key, G.Down <- keystate = (1,c,dp)
handleInput e w = w

stepWorld :: Float -> World -> World
stepWorld f (action,c,dp) = (0.5,c,step c action dp)

main :: IO ()
main = let window = InWindow "Inverted Double Pendulum" (floor windowW,floor windowH) (100,100)
       in play window 
               white 
               60 
               (0.5,defaultDPConfig,initDP defaultDPConfig)
               drawDP
               handleInput
               stepWorld

fmtFloat :: Float -> Int -> String
fmtFloat f n = showFFloat (Just n) f ""
