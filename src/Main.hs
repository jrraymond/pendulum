{-# LANGUAGE PatternGuards #-}

import qualified DoublePole as DP
import qualified InvertedPendulum as IP

import Control.Lens
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as G
import Numeric

type DPWorld = (Float,DP.Config,DP.State)
type IPWorld = (Float,IP.Config,IP.State)

windowW = 800
windowH = 400
scaleL = 10
scaleM = 10

main :: IO ()
main = let window = InWindow "Inverted Double Pendulum" (floor windowW,floor windowH) (100,100)
       in ipPlay window white 60 


ipPlay w c rate = play w c rate
                       (0.5,IP.defaultConfig,IP.initState IP.defaultConfig)
                       ipDraw
                       ipHandleInput
                       ipStepWorld


dpPlay w c rate = play w c rate 
                       (0.5,DP.defaultConfig,DP.initState DP.defaultConfig)
                       dpDraw dpHandleInput dpStepWorld


ipDraw :: IPWorld -> Picture
ipDraw (_,c,st) = Pictures [equator,cart,pole,ball,info] where
  cposx = IP.getCartPos st
  theta = IP.getTheta st
  cartW = scaleM * (c^.IP.cartMass)
  cartH = scaleM / 2 * (c^.IP.cartMass)
  b1x = cposx + scaleL * (c^.IP.pendulumLength) * sin theta
  b1y = scaleL * (c^.IP.pendulumLength) * cos theta
  cart = translate cposx 0 (rectangleWire cartW cartH)
  pole = line [(cposx,0),(b1x,b1y)]
  ball = translate b1x b1y $ Circle (scaleM * (c^.IP.pendulumMass))
  equator = line [(negate windowW,0),(windowW,0)]
  info = Translate (windowW/(-2) + 10) (windowH/(-2) + 10) $ Scale 0.1 0.1 $ 
          Text $ "cart_x: " ++ fmtFloat cposx 4 ++ " | theta: " ++ fmtFloat theta 4


ipHandleInput :: G.Event -> IPWorld -> IPWorld
ipHandleInput e w = w


ipStepWorld :: Float -> IPWorld -> IPWorld
ipStepWorld t (a,c,st) = (a,c,IP.step c 0.5 st)


dpDraw :: DPWorld -> Picture
dpDraw (_,c,dp) = Pictures [equator,cart,pole1,ball1,pole2,ball2,info] where
  cposx = dp^.DP.cartPos
  cartW = scaleM * (c^.DP.cartMass)
  cartH = scaleM / 2 * (c^.DP.cartMass)
  b1x = cposx + scaleL * (c^.DP.poleLength1) * sin (dp^.DP.theta1)
  b1y = scaleL * (c^.DP.poleLength1) * cos (dp^.DP.theta1)
  b2x = cposx + scaleL * (c^.DP.poleLength2) * sin (dp^.DP.theta2)
  b2y = scaleL * (c^.DP.poleLength2) * cos (dp^.DP.theta2)
  cart = translate cposx 0 (rectangleWire cartW cartH)
  pole1 = line [(cposx,0),(b1x,b1y)]
  ball1 = translate b1x b1y $ Scale scaleM scaleM $ Circle (10 * (c^.DP.poleMass1))
  pole2 = line [(cposx,0),(b2x,b2y)]
  ball2 = translate b2x b2y $ Scale scaleM scaleM $ Circle (10 * (c^.DP.poleMass2))
  equator = line [(negate windowW,0),(windowW,0)]
  info = Translate (windowW/(-2) + 10) (windowH/(-2) + 10) $ Scale 0.1 0.1 $ 
          Text $ "cart_x: " ++ fmtFloat cposx 4 ++ " | theta 1: " ++ 
                fmtFloat (dp^.DP.theta1) 4 ++ " | theta2: " ++ fmtFloat (dp^.DP.theta2) 4



dpHandleInput :: G.Event -> DPWorld -> DPWorld
dpHandleInput (G.EventKey key keystate _ _) (_,c,dp) 
  | G.SpecialKey G.KeyLeft  <- key, G.Down <- keystate = (0,c,dp)
  | G.SpecialKey G.KeyRight <- key, G.Down <- keystate = (1,c,dp)
dpHandleInput e w = w


dpStepWorld :: Float -> DPWorld -> DPWorld
dpStepWorld t (action,c,dp) = (0.5,c,DP.step c action dp)


fmtFloat :: Float -> Int -> String
fmtFloat f n = showFFloat (Just n) f ""
