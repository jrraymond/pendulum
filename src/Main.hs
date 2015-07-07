import DoublePole
import Control.Lens
import Graphics.Gloss

type World = (DPConfig, DoublePole)

cartW = 400
cartH = 50
windowW = 800
windowH = 400

drawDP :: World -> Picture
drawDP (c,dp) = Pictures [equator,cart,pole1,ball1,pole2,ball2] where
  cposx = dp^.cartPos
  cCenterX = cposx - 0.5 * cartW
  b1x = cposx + (c^.poleLength1) * sin (dp^.theta1)
  b1y = (c^.poleLength1) * cos (dp^.theta1)
  b2x = cposx + (c^.poleLength2) * sin (dp^.theta2)
  b2y = (c^.poleLength2) * cos (dp^.theta2)
  cart = translate cCenterX 0 (rectangleWire cartW cartH)
  pole1 = line [(cCenterX,0),(b1x,b1y)]
  ball1 = translate b1x b1y $ Circle 10
  pole2 = line [(cCenterX,0),(b2x,b2y)]
  ball2 = translate b2x b2y $ Circle 30
  equator = line [(negate windowW,0),(windowW,0)]


--handleInput :: Event -> World -> World
handleInput e w = w

stepWorld :: Float -> World -> World
stepWorld f w = w

main :: IO ()
main = let window = InWindow "Inverted Double Pendulum" (floor windowW,floor windowH) (100,100)
       in play window 
               white 
               60 
               (defaultDPConfig,initDP defaultDPConfig)
               drawDP
               handleInput
               stepWorld
