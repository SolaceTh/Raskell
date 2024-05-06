import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Random
import Data.Char

type Point  = (GLfloat , GLfloat)

data Shape = Pnt Point
           | Line' Point Point
           | Tri Point Point Point
           | Rectangle Float Float Point
           | Circle Float Point
           | Fractal Shape Int
           | HypnoFractal Float Float
    deriving (Show, Eq)

data Token = Flt Float
           | LPar | RPar | LBra | RBra | Comma
           | Err String | PS Shape | Shapes [Shape]
    deriving (Show, Eq)

lexer :: String -> [Token]
--Exit
lexer ""       = []
--Puncuation
lexer ('(':xs) = LPar  : lexer xs 
lexer (')':xs) = RPar  : lexer xs
lexer ('[':xs) = LBra  : lexer xs
lexer (']':xs) = RBra  : lexer xs
lexer (',':xs) = Comma : lexer xs
--Numbers and Errors
lexer (x:xs)
 | isDigit x   = let (y, ys) = span isDigit xs
                     (z, zs) = span isDigit $ tail ys
                 in Flt ((read (x:y)) + (read (z)) / 10 ^ (length z)) : lexer zs
 | isSpace x   = lexer xs
lexer xs       = [Err ("Cannot Tokenize : " ++ [(head xs)])]

parser :: [Token] -> Either [Shape] String
parser tokens =
    case sr [] tokens of 
        [Shapes s]  -> Left s
        [PS     s]  -> Left [s]
        [Err e]     -> Right $ "Lexical Error : " ++ e
        err         -> Right $ "Parse Error : "   ++ show err

sr :: [Token] -> [Token] -> [Token]
--Shapes
sr (RPar : Flt f1      : Comma       : Flt f0      : LPar : tokens) q = sr (PS      (Pnt (f0, f1))       : tokens) q
sr (RBra : PS (Pnt p1) : PS (Pnt p0)               : LBra : tokens) q = sr (PS       (Line' p0 p1)       : tokens) q
sr (RBra : PS (Pnt p2) : PS (Pnt p1) : PS (Pnt p0) : LBra : tokens) q = sr (PS       (Tri p0 p1 p2)      : tokens) q
sr (RBra : PS (Pnt p)  : Flt f1      : Flt f0      : LBra : tokens) q = sr (PS    (Rectangle f0 f1 p)    : tokens) q
sr (RBra : PS (Pnt p)  : Flt f                     : LBra : tokens) q = sr (PS       (Circle f p)        : tokens) q
sr (RBra : Flt f       : PS shape                  : LBra : tokens) q = sr (PS (Fractal shape $ floor f) : tokens) q
--Concatination
sr (PS s1 : PS s0     : tokens) q
 | elem LBra tokens               = sr (head q : PS s1 : PS s0 : tokens) (tail q)
 | otherwise                      = sr (Shapes    [s0, s1]     : tokens) q
sr (PS s  : Shapes ss : tokens) q = sr (Shapes   (ss ++ [s])   : tokens) q
--Shift
sr s (token:q) = sr (token:s) q
--Exit
sr (Err e : tokens) q = [Err e]
sr tokens          [] = tokens

draw :: [Shape] -> IO ()
draw [] = putStrLn "Finished"
draw (Pnt p : shapes) = do
    drawPoint p
    draw shapes
draw (Line' p0 p1 : shapes) = do
    drawLine p0 p1
    draw shapes
draw (Tri p0 p1 p2 : shapes) = do
    drawTriangle (Tri p0 p1 p2)
    draw shapes
draw (Rectangle f0 f1 p : shapes) = do
    drawRectangle (Rectangle f0 f1 p)
    draw shapes
draw (Circle f p : shapes) = do
    drawCircle (Circle f p)
    draw shapes
draw (Fractal s n : shapes) = do
    drawFractal s n
    draw shapes

drawLine :: Point -> Point -> IO ()
drawLine (x1, y1) (x2, y2) = do
    renderPrimitive Lines $ do
        vertex $ Vertex2 x1 y1
        vertex $ Vertex2 x2 y2

drawPoint :: Point -> IO ()
drawPoint (x, y) = do
  renderPrimitive Points $ do
    color $ Color3 1.0 1.0 (1.0 :: GLfloat)
    vertex $ Vertex2 x y

drawFractal :: Shape -> Int -> IO ()
drawFractal (Tri p0 p1 p2) n
    | n <= 11   = drawTriangleFractal  (Tri p0 p1 p2) n
    | otherwise = putStrLn "Too large of an n. Will be 2^n operations."
drawFractal (Rectangle f0 f1 p) n
    | n <= 11   = drawRectangleFractal (Rectangle f0 f1 p) n
    | otherwise = putStrLn "Too large of an n. Will be 2^n operations."
drawFractal s _ = putStrLn $ "Cannot create a fractal from " ++ show s

line :: Point -> Point -> [Point]--change
line (x1, y1) (x2, y2) = line' x1 y1 x2 y2
    where
        line' :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> [Point]
        line' x y x2 y2
         | x > x2    = []
         | otherwise = (x, y) : line' (x + 1) newY x2 y2
            where
                dx   = x2 - x
                dy   = y2 - y
                sx   = if dx > 0 then 1 else -1
                sy   = if dy > 0 then 1 else -1
                p    = 2 * abs(dy) - abs(dx)
                newY = if p < 0 then y else y + sy

drawCircle :: Shape -> IO () -- circle
drawCircle (Circle radius center) = do
    let numSegments = 100
        angleIncrement = 2 * pi / fromIntegral numSegments
        points = [(centerX + radius * cos (angleIncrement * fromIntegral i), centerY + radius * sin (angleIncrement * fromIntegral i)) | i <- [0..numSegments]]
        (centerX, centerY) = center
    color $ Color3 0.0 0.0 (1.0 :: GLfloat)
    renderPrimitive Lines $ do
        mapM_ (\(p1, p2) -> do
            vertex $ Vertex2 (fst p1) (snd p1)
            vertex $ Vertex2 (fst p2) (snd p2)) (zip points (tail points))

drawRectangle :: Shape -> IO ()
drawRectangle (Rectangle width height bottomLeft) = do
    let topLeft = (fst bottomLeft, snd bottomLeft + height)
        topRight = (fst topLeft + width, snd topLeft)
        bottomRight = (fst topRight, snd bottomLeft)
    color $ Color3 1.0 0.0 (0.0 :: GLfloat)
    renderPrimitive Lines $ do
        mapM_ (\(p1, p2) -> drawLine p1 p2)
            [ (bottomLeft, topLeft)
            , (topLeft, topRight)
            , (topRight, bottomRight)
            , (bottomRight, bottomLeft)]

drawTriangle :: Shape -> IO ()
drawTriangle (Tri p1 p2 p3) = do
    color $ Color3 0.0 (1.0 :: GLfloat) 0.0
    renderPrimitive Lines $ do
        mapM_ (\(p1', p2') -> drawLine p1' p2')
            [ (p1, p2)
            , (p2, p3)
            , (p3, p1)]

drawRectangleFractal :: Shape -> Int -> IO ()
-- Base case: draw a single rectangle
drawRectangleFractal rect 0 = drawRectangle rect  
drawRectangleFractal (Rectangle width height (x, y)) n = do
    -- Draw left half
    drawRectangle (Rectangle (width / 2) height (x, y))
    -- Draw right half
    drawRectangle (Rectangle (width / 2) height (x + width / 2, y))
    -- Recursively draw top halves
    drawRectangleFractal (Rectangle (width / 2) (height / 2) (x, y + height / 2)) (n - 1)
    -- Recursively draw bottom halves
    drawRectangleFractal (Rectangle (width / 2) (height / 2) (x + width / 2, y + height / 2)) (n - 1)


drawTriangleFractal :: Shape -> Int -> IO ()
-- Base case: draw a single triangle
drawTriangleFractal tri 0 = drawTriangle tri
drawTriangleFractal (Tri p1 p2 p3) n = do
    drawTriangleFractal (Tri p1 midP1 midP3) (n - 1)  -- Draw left sub-triangle
    drawTriangleFractal (Tri midP1 p2 midP2) (n - 1)  -- Draw top sub-triangle
    drawTriangleFractal (Tri midP3 midP2 p3) (n - 1)  -- Draw right sub-triangle
  where
    midP1 = ((fst p1 + fst p2) / 2, (snd p1 + snd p2) / 2)
    midP2 = ((fst p2 + fst p3) / 2, (snd p2 + snd p3) / 2)
    midP3 = ((fst p1 + fst p3) / 2, (snd p1 + snd p3) / 2)

displayCanvas :: String -> DisplayCallback
displayCanvas inp = do
    clear [ColorBuffer]
    case parser (lexer inp) of
        Left  s -> draw s
        Right e -> putStrLn e
    swapBuffers

displayCanvas' :: Shape -> DisplayCallback
displayCanvas' s = do
    clear [ColorBuffer]
    draw [s]
    swapBuffers

initCanvas :: Either String Shape -> IO ()
initCanvas s = do
    (_progName, _args) <- getArgsAndInitialize
    initialWindowSize $= Size 800 800  -- Set the window size to 4K resolution
    _window <- createWindow "OpenGL Window"
    case s of
        Left  s -> displayCallback $= displayCanvas  s
        Right s -> displayCallback $= displayCanvas' s
    ortho2D 0 2 0 2
    mainLoop

main :: IO ()
main = repl

repl :: IO ()
repl = do 
    putStrLn "Welcome! Please choose an option :\n1. Load\n2. New\n3. Quit"
    choice0 <- getLine
    case choice0 of
        "1" -> do
            putStrLn "Please enter a file name :"
            fname <- getLine
            inp   <- readFile fname
            putStrLn "Loading..."
            initCanvas $ Left inp
            displayCanvas inp
            repl
        "2" -> do
            putStrLn "What would you like to draw?\n1. Point\n2. Line\n3. Triangle\n4. Rectangle\n5. Circle\n6. Fractal\n7. Back"
            choice1 <- getLine
            case choice1 of
                "1" -> pointHandler               
                "2" -> lineHandler
                "3" -> triHandler
                "4" -> rectHandler
                "5" -> circHandler
                "6" -> fractHandler
                "7" -> repl
        "3" -> do
            putStrLn "Bye!"

pointHandler :: IO ()
pointHandler = do
    putStrLn "Please input the x value of the point :"
    x <- getLine
    putStrLn "Please input the y value of the point :"
    y <- getLine
    putStrLn "Drawing..."
    initCanvas $ Right $ Pnt (read x, read y)
    displayCanvas'     $ Pnt (read x, read y)

lineHandler :: IO ()
lineHandler = do
    putStrLn "Please input the x value of the first point :"
    x0 <- getLine
    putStrLn "Please input the y value of the first point :"
    y0 <- getLine
    putStrLn "Please input the x value of the second point :"
    x1 <- getLine
    putStrLn "Please input the y value of the second point :"
    y1 <- getLine
    putStrLn "Drawing..."
    initCanvas $ Right $ Line' (read x0, read y0) (read x1, read y1)
    displayCanvas'     $ Line' (read x0, read y0) (read x1, read y1)

triHandler :: IO ()
triHandler = do
    putStrLn "Please input the x value of the first vertex :"
    x0 <- getLine
    putStrLn "Please input the y value of the first vertex :"
    y0 <- getLine
    putStrLn "Please input the x value of the second vertex :"
    x1 <- getLine
    putStrLn "Please input the y value of the second vertex :"
    y1 <- getLine
    putStrLn "Please input the x value of the third vertex :"
    x2 <- getLine
    putStrLn "Please input the y value of the third vertex :"
    y2 <- getLine
    putStrLn "Drawing..."
    initCanvas $ Right $ Tri (read x0, read y0) (read x1, read y1) (read x2, read y2)
    displayCanvas'     $ Tri (read x0, read y0) (read x1, read y1) (read x2, read y2)

rectHandler :: IO ()
rectHandler = do
    putStrLn "Please input the length :"
    l <- getLine
    putStrLn "Please input the width :"
    w <- getLine
    putStrLn "Please input the x value of the origin :"
    x <- getLine
    putStrLn "Please input the y value of the origin :"
    y <- getLine
    putStrLn "Drawing..."
    initCanvas $ Right $ Rectangle (read l) (read w) (read x, read y)
    displayCanvas'     $ Rectangle (read l) (read w) (read x, read y)

circHandler :: IO ()
circHandler = do
    putStrLn "Please input the radius :"
    r <- getLine
    putStrLn "Please input the x value of the origin :"
    x <- getLine
    putStrLn "Please input the y value of the origin :"
    y <- getLine
    putStrLn "Drawing..."
    initCanvas $ Right $ Circle (read r) (read x, read y)
    displayCanvas'     $ Circle (read r) (read x, read y)

fractHandler :: IO ()
fractHandler = do
    putStrLn "Please choose a shape with which to construct a fractal :\n1. Rectangle\n2. Triangle"
    choice2 <- getLine
    case choice2 of
        "1" -> do
            putStrLn "Please input the length :"
            l <- getLine
            putStrLn "Please input the width :"
            w <- getLine
            putStrLn "Please input the x value of the origin :"
            x <- getLine
            putStrLn "Please input the y value of the origin :"
            y <- getLine
            putStrLn "Please enter the number of reiterations :"
            n <- getLine
            putStrLn "Drawing..."
            initCanvas $ Right $ Fractal (Rectangle (read l) (read w) (read x, read y)) (read n)
            displayCanvas'     $ Fractal (Rectangle (read l) (read w) (read x, read y)) (read n)
        "2" -> do
            putStrLn "Please input the x value of the first vertex :"
            x0 <- getLine
            putStrLn "Please input the y value of the first vertex :"
            y0 <- getLine
            putStrLn "Please input the x value of the second vertex :"
            x1 <- getLine
            putStrLn "Please input the y value of the second vertex :"
            y1 <- getLine
            putStrLn "Please input the x value of the third vertex :"
            x2 <- getLine
            putStrLn "Please input the y value of the third vertex :"
            y2 <- getLine
            putStrLn "Please input the number of reiterations :"
            n <- getLine
            putStrLn "Drawing..."
            initCanvas $ Right $ Fractal (Tri (read x0, read y0) (read x1, read y1) (read x2, read y2)) (read n)
            displayCanvas'     $ Fractal (Tri (read x0, read y0) (read x1, read y1) (read x2, read y2)) (read n)
