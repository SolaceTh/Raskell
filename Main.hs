import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

type Point = (GLfloat, GLfloat)

-- Define the corners of the rectangle
rectangle :: [Point]
rectangle =
  [ (-0.5, -0.5), -- Bottom left
    (-0.5, 0.5),  -- Top left
    (0.5, 0.5),   -- Top right
    (0.5, -0.5)   -- Bottom right
  ]

-- Generate points within the rectangle
fillRectangle :: [Point]
fillRectangle = [(x, y) | x <- [-0.5, -0.4955 .. 0.5], y <- [-0.5, -0.4955 .. 0.5]]

-- Generate points for a circle
circle :: Float -> [Point]
circle radius =
  let numPoints = 500 
      angleIncrement = 2 * pi / fromIntegral numPoints
  in [(radius * cos (angleIncrement * fromIntegral i), radius * sin (angleIncrement * fromIntegral i)) | i <- [0 .. numPoints - 1]]

drawRectangle :: IO ()
drawRectangle = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "OpenGL Rectangle"
  displayCallback $= displayRectangle
  windowSize $= Size 800 600
  ortho (-1) 1 (-1) 1 (-1) 1
  mainLoop

drawCircle :: IO ()
drawCircle = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "OpenGL Circle"
  displayCallback $= displayCircle
  windowSize $= Size 800 600
  ortho (-1) 1 (-1) 1 (-1) 1
  mainLoop

displayRectangle :: DisplayCallback
displayRectangle = do
  clear [ColorBuffer]
  renderPrimitive Points $
    mapM_ (\(x, y) -> vertex $ Vertex2 x y) fillRectangle
  swapBuffers

displayCircle :: DisplayCallback
displayCircle = do
  clear [ColorBuffer]
  renderPrimitive Points $
    mapM_ (\(x, y) -> vertex $ Vertex2 x y) (circle 0.5)
  swapBuffers

main :: IO ()
main = drawCircle
