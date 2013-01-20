import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Colors
import Graphics.UI.GLFW as GLFW
import Graphics.GLUtil.Shaders as S

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
  pointSmooth $= Enabled
  soundSource <- P.simpleNew Nothing "X" P.Record Nothing "visualizer" (P.SampleSpec (P.F32 P.LittleEndian) 4096 2) Nothing Nothing

  -- shading
  vs <- S.loadShader "vshader.s"
  fs <- S.loadShader "fshader.s"
  p <- linkShaderProgram [vs] [fs]
  currentProgram $= (Just p)

  uL <- get (uniformLocation p "A")
  uniform uL $= Index1 (1.0::GLfloat)

  sampleLoop soundSource 1 uL

sampleLoop source count ul = do
  let additiveIterations = 1
  when (count == additiveIterations) $ do clear [ColorBuffer]
  _ <- (P.simpleRead source $ 100 :: IO [GLfloat]) >>= renderS ul >> return ()
  GLFW.swapBuffers
  sampleLoop source (if count == additiveIterations then 1 else count + 1) ul


rangeX = [-50..0]++[1..50]

renderS ul sampleS = do 
  let max' = foldl max 0 sampleS
  uniform ul $= Index1 (max' :: GLfloat)
  renderPrimitive LineStrip $ forM (zip sampleS (rangeX)) $ \(sample,p) -> do
    colorize (sin sample) (p/100) 0.5
    vertify (p/100) (sample) 0.0
  return ()
