import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Colors
import Graphics.UI.GLFW as GLFW
import Graphics.GLUtil.Shaders as S

import Sound.Pulse.Simple as P

import Control.Monad
import System.Random
import Data.List

vertify :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertify x y z = vertex $ Vertex3 x y z

colorize :: GLfloat -> GLfloat -> GLfloat -> IO ()
colorize r g b = color $ Color3 r g b


sampleRate = 800
sampleBuffer = round . head . dropWhile (((fromIntegral sampleRate) * 5.12) >) $ iterate (*2) 2


main = do 
  isInitialized <- GLFW.initialize
  isWindowOpened <- openWindow (Size 800 400) [] Window
  windowTitle $= "Visualize this"
  pointSize $= 2
  pointSmooth $= Enabled
  soundSource <- P.simpleNew Nothing "X" P.Record Nothing "visualizer" (P.SampleSpec (P.F32 P.LittleEndian) sampleBuffer 2) Nothing Nothing
  sampleLoop soundSource 1 

sampleLoop source count = do
  let additiveIterations = 1
  when (count == additiveIterations) $ do clear [ColorBuffer]
  _ <- (P.simpleRead source $ sampleRate :: IO [GLfloat]) >>= renderS >> return ()
  GLFW.swapBuffers
  sampleLoop source (if count == additiveIterations then 1 else count + 1) 


rangeX = [-200..0]++[1..200]

drawSquare = \(width, height, posX, posY) (sample, random) -> do
    colorize (sin sample) 0.0 0.0
    vertify (posX - width/2) (posY + height/2) 0.0
    vertify (posX - width/2) (posY - height/2) 0.0
    vertify (posX + width/2) (posY - height/2) 0.0
    vertify (posX + width/2) (posY + height/2) 0.0
 
squareDrawR :: (GLfloat, GLfloat, GLfloat, GLfloat) -> [(GLfloat, GLfloat)] -> IO ()
squareDrawR _ [] = return ()
squareDrawR (w, h, x, y) samples = do
    rGen <- newStdGen
    let iterateP x y = iterate (+x) y
    let iterateM x y = iterate (*x) y
    let iterateR x y = randomRs(x,y) rGen
    let iterateF     = unfoldr (\(a,b) -> Just (a,(b,a+b))) (0,1)

    let positions = concatMap
          (\d -> [((x - w/d), (y + h/d)),((x - w/d),(y - h/d)),((x + w/d),(y - h/d)),((x + w/d),(y + h/d))])
          $ iterateF 
    forM_ (zip samples (iterate (+1) 2)) $ \(sample@(_,r), div) -> do
      -- rGen <- newStdGen
      -- let positions' = nub . map (positions !!) . take 20 $ randomRs (0,40) rGen
      mapM_ (\(x,y) -> drawSquare ((w/div, h/div, x, y)) sample) $ take 40 positions

continuationDraw _ _ _ [] = return ()
continuationDraw (x,y) d m (sample:samples) =
  let m' | m < sample = sample | otherwise = m
      x' = (x + sample)/10
      y' = (y + sample)/10
  in do
    colorize (sin m' / 3.14 ) (let g = cos x' / 2 in if x' > 0.0 then 0.0 else g) (let b = sin x' / 2 in if x' < 0.0 then 0.0 else x')
    case d of 1 -> vertify (x) (y + sin sample) 0.0
              2 -> vertify (x + sin sample) (y) 0.0
              3 -> vertify (x) (y - sin sample) 0.0
    let d' | d == 3 = 1 | otherwise = d + 1
    continuationDraw (x', y') d' m' samples
  
renderS sampleS = do 
  -- rGen <- newStdGen
  -- renderPrimitive LineStrip $ squareDrawR (2.0, 2.0, 0.0, 0.0) (zip sampleS (randomRs (0,100) rGen))
  renderPrimitive Lines $ continuationDraw (0.0, 0.0) 1 0.0 sampleS
  return ()
