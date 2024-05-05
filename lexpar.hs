import Data.Char

type Point  = (Float, Float)

data Shape = Pnt Point
           | Line Point Point
           | Triangle Point Point Point
           | Rectangle Float Float Point
           | Circle Float Point
           | Fractal Shape Int
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


p = "(12.2, 3.4)"
l = "[(1.4, 1.5) (2.0, 5.4)]"
tr = "[(5.2, 6.1) (1.4, 5.8) (9.8, 7.8)]"
rect = "[4.0 5.0 (2.3, 1.2)]"
circ = "[8.0 (1.9, 2.875)]"
fract = "[[4.0 5.0 (2.3, 1.2)] 5.0]"
full = "(12.2, 3.4) [(1.4, 1.5) (2.0, 5.4)] [(5.2, 6.1) (1.4, 5.8) (9.8, 7.8)] [4.0 5.0 (2.3, 1.2)] [8.0 (1.9, 2.875)] [[4.0 5.0 (2.3, 1.2)] 5.0]"

parser :: [Token] -> Either [Shape] String
parser tokens =
    case sr [] tokens of 
        [Shapes s]  -> Left s
        [Err e]     -> Right $ "Lexical Error : " ++ e
        err         -> Right $ "Parse Error : "   ++ show err

sr :: [Token] -> [Token] -> [Token]
--Shapes
sr (RPar : Flt f1      : Comma       : Flt f0      : LPar : tokens) q = sr (PS      (Pnt (f0, f1))       : tokens) q
sr (RBra : PS (Pnt p1) : PS (Pnt p0)               : LBra : tokens) q = sr (PS       (Line p0 p1)        : tokens) q
sr (RBra : PS (Pnt p2) : PS (Pnt p1) : PS (Pnt p0) : LBra : tokens) q = sr (PS    (Triangle p0 p1 p2)    : tokens) q
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
draw (Pnt p      : shapes) = do
                        drawPoint p
                        draw shapes
draw (Line p0 p1 : shapes) = do
                        drawLine p0 p1
                        draw shapes
draw (Triangle p0 p1 p2 : shapes) = do
                                drawTriangle p0 p1 p2
                                draw shapes
draw (Rectangle f0 f1 p : shapes) = do
                                drawRectangle f0 f1 p
                                draw shapes
draw (Circle f p        : shapes) = do
                                drawCircle f p
                                draw shapes
draw (Fractal s n       : shapes) = do
                                drawFractal s n
                                draw shapes