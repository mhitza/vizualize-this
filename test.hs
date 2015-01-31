import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Colors
import Graphics.UI.GLFW as GLFW
import Graphics.GLUtil.Shaders as S

import Sound.Pulse.Simple as P

import Data.Time.Clock.POSIX
import Control.Monad
import System.Random
import Data.List

vertify :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertify x y z = vertex $ Vertex3 x y z

colorize :: GLfloat -> GLfloat -> GLfloat -> IO ()
colorize r g b = color $ Color3 r g b

iterateP x = iterate (+x)
iterateM x = iterate (*x)

-- There is some kind of "magic" relation between the sample rate and the sample buffer
-- How this is now considered is by taking the power of 2 > sampleRate * 5.12
-- This may very well be specific to the computer it is run on.
-- If the number is substantially greater/lower than according to this formula, big delays take place.
sampleRate = 800
sampleBuffer = round . head . dropWhile ((fromIntegral sampleRate * 5.12) >) $ iterateM 2 2

main = do 
  isInitialized <- GLFW.initialize
  isWindowOpened <- openWindow (Size 800 400) [] Window
  windowTitle $= "Visualize this"
  pointSize $= 2
  pointSmooth $= Enabled
  soundSource <- P.simpleNew Nothing "X" P.Record Nothing "visualizer" (P.SampleSpec (P.F32 P.LittleEndian) sampleBuffer 2) Nothing Nothing
  sampleLoop soundSource 1 0

sampleLoop source count t = do
  let total = t + 1
  -- how many draws should take place before the screen is cleared
  let additiveIterations = 1
  when (count == additiveIterations) $ clear [ColorBuffer]
  renderS total =<< (P.simpleRead source sampleRate :: IO [GLfloat])
  GLFW.swapBuffers
  sampleLoop source (if count == additiveIterations then 1 else count + 1)  total

-- A range reprezenting the X axis points, tied to the sample rate. Used in visualization
-- where each sample point is given an X axis corespondent. 800 samples, 400 on the left 
-- of the 0 position, and 400 on the right (actually off by one, but disregard that)
-- Used just in the last example to draw an oscilloscope
rangeX = [-400..0]++[1..400]

drawSquare (width, height, posX, posY) (sample, random) = do
    colorize (sin sample) 0.0 0.0
    vertify (posX - width/2) (posY + height/2) 0.0
    vertify (posX - width/2) (posY - height/2) 0.0
    vertify (posX + width/2) (posY - height/2) 0.0
    vertify (posX + width/2) (posY + height/2) 0.0
 
squareDrawR :: (GLfloat, GLfloat, GLfloat, GLfloat) -> [(GLfloat, GLfloat)] -> IO ()
squareDrawR _ [] = return ()
squareDrawR (w, h, x, y) samples = do
    rGen <- newStdGen
    let iterateR x y = randomRs(x,y) rGen
    let iterateF     = unfoldr (\(a,b) -> Just (a,(b,a+b))) (0,1)

    -- experimenting with square drawing by generating sample points via different
    -- iterate functions. iterateM/iterateP/iterateR/iterateF
    let positions = concatMap
          (\d -> [
            (x - w / d, y + h / d),
            (x - w / d, y - h / d),
            (x + w / d, y - h / d),
            (x + w / d, y + h / d)
            ])
          $ iterateM 2 4
    forM_ (zip samples (iterateP 1 2)) $ \(sample@(_,r), div) -> do
      rGen <- newStdGen
      -- pick 20 random positions in the range (0,40) then only get the unique ones
      let positions' = nub . map (positions !!) . take 20 $ randomRs (0,40) rGen
      mapM_ (\(x,y) -> drawSquare (w / div, h / div, x, y) sample) $ take 20 positions

continuationDraw _ _ _ [] = return ()
continuationDraw (x,y) d m (sample:samples) =
  let m' | m < sample = sample | otherwise = m
      x' = (x + sample)/10
      y' = (y + sample)/10
  in do
    colorize (sin m' / 3.14 ) (let g = cos x' / 2 in if x' > 0.0 then 0.0 else g) (let b = sin x' / 2 in if x' < 0.0 then 0.0 else x')
    -- direction in which the next point will go based on sin of sample. 1:Top, 2:Right, 3:Down
    case d of 1 -> vertify x (y + sin sample) 0.0
              2 -> vertify (x + sin sample) y 0.0
              3 -> vertify x (y - sin sample) 0.0
    let d' | d == 3 = 1 | otherwise = d + 1
    continuationDraw (x', y') d' m' samples
  
renderS count sampleS = do 
   cp <- get currentProgram
   case cp of
       Nothing -> return ()
       Just p  -> do 
                     tl <- get (uniformLocation p "time")
                     time <- liftM (fromIntegral . floor) getPOSIXTime
                     uniform tl $= Index1 (time :: GLfloat)
                     cl <- get (uniformLocation p "count")
                     uniform cl $= Index1 (count :: GLfloat)
                     return ()
   rGen <- newStdGen
   --renderPrimitive Quads $ squareDrawR (2.0, 2.0, 0.0, 0.0) (zip sampleS (randomRs (0,100) rGen))
   renderPrimitive LineStrip $ continuationDraw (0.0, 0.0) 1 0.0 sampleS
   --renderPrimitive LineStrip $ forM (zip sampleS rangeX) $ \(sample, x) -> do
   --  colorize 1.0 0.0 0.0
   --  vertify (x/400) (sin sample) 0.0
