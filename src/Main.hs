{-# LANGUAGE PatternGuards #-}

import qualified DoublePole as DP
import qualified SinglePole as SP

import Control.Lens
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as G
import Numeric
import Options.Applicative

type DPWorld = (Ordering,DP.Config,DP.State)
type SPWorld = (Ordering,SP.Config,SP.State)

windowW = 1200
windowH = 800
ppm = 400  --pixels per meter

main :: IO ()
main = execParser opts >>= startPlay
  where opts = info (helper <*> optsParser)
                    (fullDesc <> progDesc "Balance (an) inverted pendulum(s)"
                              <> header "Inverted Pendulum")
   

startPlay :: (Bool,Bool) -> IO ()
startPlay (b,c) = let background | c = black | otherwise = white
                      foreground | c = white | otherwise = black
                      rate = 60
                      window = InWindow "Inverted Pendulum(s)" 
                                        (floor windowW,floor windowH) 
                                        (100,100)
                  in if b then dpPlay window background foreground rate
                          else spPlay window background foreground rate


optsParser :: Parser (Bool,Bool)
optsParser = (,) <$> switch (short 'd' <> long "double" <>
                             help "Double inverted pendulum")
                 <*> switch (short 'n' <> long "night" <>
                             help "Dark background")


spPlay w bc fc rate = play w bc rate
                       (EQ,SP.defaultConfig,SP.initState SP.defaultConfig)
                       (spDraw fc)
                       spHandleInput
                       spStepWorld

dpPlay w bc fc rate = play w bc rate 
                           (EQ,DP.defaultConfig,DP.initState DP.defaultConfig)
                           (dpDraw fc)
                           dpHandleInput
                           dpStepWorld

spDraw :: Color -> SPWorld -> Picture
spDraw fc (_,c,st) = Pictures [gridx,gridy,cart,pole,ball,info] where
  cposx = SP.getPos st * ppm
  theta = SP.getTh st
  cartW = c^.SP.cartMass
  cartH = (c^.SP.cartMass) / 2
  pL = (c^.SP.poleLength) * ppm
  b1x = cposx + pL * sin theta
  b1y = pL * cos theta
  cart = Color fc $ translate cposx 0 (rectangleWire cartW cartH)
  pole = Color fc $ line [(cposx,0),(b1x,b1y)]
  ball = Color fc $ translate b1x b1y $ Circle (c^.SP.poleMass)
  gc | fc == white = greyN 0.1 | otherwise = greyN 0.9
  gridx = Color gc $ Line [(negate windowW,0),(windowW,0)]
  gridy = Color gc $ Line [(0,negate windowH),(0,windowH)]
  info = Color fc $ Translate (windowW/(-2) + 10) (windowH/(-2) + 10) 
                  $ Scale 0.1 0.1 
                  $ Text $ "cart_x: " ++ fmtFloat cposx 2 ++
                           " | vel: " ++ fmtFloat (SP.getVel st) 2 ++
                           " | theta: " ++ fmtFloat theta 2 ++
                           " | angVel: " ++ fmtFloat (SP.getAV st) 2


spHandleInput :: G.Event -> SPWorld -> SPWorld
spHandleInput (G.EventKey key keystate _ _) (_,c,st) 
  | G.SpecialKey G.KeyLeft  <- key, G.Down <- keystate = (LT,c,st)
  | G.SpecialKey G.KeyLeft  <- key, G.Up <- keystate = (EQ,c,st)
  | G.SpecialKey G.KeyRight <- key, G.Down <- keystate = (GT,c,st)
  | G.SpecialKey G.KeyRight <- key, G.Up <- keystate = (EQ,c,st)
  | G.MouseButton _ <- key = (EQ,c,SP.State [0,0,0,0])
spHandleInput e w = w


spStepWorld :: Float -> SPWorld -> SPWorld
spStepWorld t (a,c,st) 
  | a == LT = (a,c,SP.step t c 0 st)
  | a == GT = (a,c,SP.step t c 1 st)
  | otherwise = (a,c,SP.step t c 0.5 st)


dpDraw :: Color -> DPWorld -> Picture
dpDraw fc (_,c,dp) = Pictures [gridx,gridy,cart,pole1,ball1,pole2,ball2,info] where
  gc | fc == white = greyN 0.1 | otherwise = greyN 0.9
  cposx = DP.getPos dp * ppm
  cartW = c^.DP.cartMass
  cartH = (c^.DP.cartMass) / 2
  pL1 = (c^.DP.poleLength1) * ppm
  pL2 = (c^.DP.poleLength2) * ppm
  b1x = cposx + pL1 * sin (DP.getTh1 dp)
  b1y = pL1 * cos (DP.getTh1 dp)
  b2x = cposx + pL2 * sin (DP.getTh2 dp)
  b2y = pL2 * cos (DP.getTh2 dp)
  cart = Color fc $ translate cposx 0 (rectangleWire cartW cartH)
  pole1 = Color fc $ line [(cposx,0),(b1x,b1y)]
  ball1 = Color fc $ translate b1x b1y $ Circle (c^.DP.poleMass1)
  pole2 = Color fc $ line [(cposx,0),(b2x,b2y)]
  ball2 = Color fc $ translate b2x b2y $ Circle (c^.DP.poleMass2)
  gridx = Color gc $ line [(negate windowW,0),(windowW,0)]
  gridy = Color gc $ Line [(0,negate windowH),(0,windowH)]
  info = Color fc $ 
         Translate (windowW/(-2) + 10) (windowH/(-2) + 10) $
         Scale 0.1 0.1 $ 
         Text $ "cart_x: " ++ fmtFloat cposx 2 ++
                " | theta 1: " ++ fmtFloat (DP.getTh1 dp) 2 ++
                " | theta2: " ++ fmtFloat (DP.getTh2 dp) 2



dpHandleInput :: G.Event -> DPWorld -> DPWorld
dpHandleInput (G.EventKey key keystate _ _) (_,c,st) 
  | G.SpecialKey G.KeyLeft  <- key, G.Down <- keystate = (LT,c,st)
  | G.SpecialKey G.KeyLeft  <- key, G.Up <- keystate = (EQ,c,st)
  | G.SpecialKey G.KeyRight <- key, G.Down <- keystate = (GT,c,st)
  | G.SpecialKey G.KeyRight <- key, G.Up <- keystate = (EQ,c,st)
  | G.MouseButton _ <- key = (EQ,c,DP.State [0,0,0,0,0,0])
dpHandleInput e w = w


dpStepWorld :: Float -> DPWorld -> DPWorld
dpStepWorld t (a,c,dp) 
  | a == LT = (a,c,DP.step t c 0 dp)
  | a == GT = (a,c,DP.step t c 1 dp)
  | a == EQ = (a,c,DP.step t c 0.5 dp)


fmtFloat :: Float -> Int -> String
fmtFloat f n = showFFloat (Just n) f ""
