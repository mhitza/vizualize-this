import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Colors
import Graphics.UI.GLFW as GLFW

import Sound.Pulse.Simple as P

import Control.Monad

vertify :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertify x y z = vertex $ Vertex3 x y z

colorize :: GLfloat -> GLfloat -> GLfloat -> IO ()
colorize r g b = color $ Color3 r g b

main = do 
  isInitialized <- GLFW.initialize
  isWindowOpened <- openWindow (Size 800 400) [] Window
  windowTitle $= "Visualize this"
  pointSize $= 2
  soundSource <- P.simpleNew Nothing "X" P.Record Nothing "visualizer" (P.SampleSpec (P.F32 P.LittleEndian) 4096 2) Nothing Nothing
  sampleLoop soundSource 1

sampleLoop source count = do
  let additiveIterations = 1
  when (count == additiveIterations) $ do clear [ColorBuffer]
  _ <- (P.simpleRead source $ 800 :: IO [GLfloat]) >>= renderS >> return ()
  GLFW.swapBuffers
  sampleLoop source (if count == additiveIterations then 1 else count + 1)


rangeX = [-400..0]++[1..400]

renderS sampleS = do 
  renderPrimitive Triangles $ forM (zip sampleS (rangeX)) $ \(sample,p) -> do
    colorize (sin sample) (p/200) 0.5
    vertify (p/200) (sin sample) 0.0
  return ()
