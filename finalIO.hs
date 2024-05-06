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
    draw s
    swapBuffers

initCanvas :: IO ()
initCanvas = do
    (_progName, _args) <- getArgsAndInitialize
    initialWindowSize $= Size 3840 2160  -- Set the window size to 4K resolution
    _window <- createWindow "OpenGL Window"
    displayCallback $= displayCanvas
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
            displayCanvas inp
            initCanvas
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
    displayCanvas' $ Pnt (x, y)
    initCanvas
    repl

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
    putStrLn "Drawing"
    displayCanvas' $ Line' (x0, y0) (x1, y1)
    initCanvas
    repl
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
    displayCanvas' $ Tri (x0, y0) (x1, y1) (z1, z2)
    initCanvas
    repl

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
    displayCanvas' $ Rectangle l w (x, y)
    initCanvas
    repl

circHandler :: IO ()
circHandler = do
    putStrLn "Please input the radius :"
    r <- getLine
    putStrLn "Please input the x value of the origin :"
    x <- getLine
    putStrLn "Please input the y value of the origin :"
    y <- getLine
    putStrLn "Drawing..."
    displayCanvas' $ Circ r (x, y)
    initCanvas
    repl

fractHandler :: IO ()
fractHandler = do
    putStrLn "Please choose a shape with which to construct a fractal :\n1. Triangle\n2. Rectangle"
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
            displayCanvas' $ Fractal (Rectangle l w (x, y)) n
            initCanvas
            repl
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
            displayCanvas' $ Fractal (Triangle (x0, y0) (x1, y1) (x2, y2)) n
            initCanvas
            repl