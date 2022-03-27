-- 11.2 Basic declarations <2021-12-15 mer.>

-- [[file:../notes.org::*11.2 Basic declarations <2021-12-15 mer.>][11.2 Basic declarations <2021-12-15 mer.>:1]]
import Data.Char
import Data.List
import System.IO

-- clear the screen
cls :: IO ()
cls = putStr "\ESC[2J"

-- coordinates
type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

size :: Int
size = 3

type Grid = [[Player]]
data Player = O | B | X deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O
-- 11.2 Basic declarations <2021-12-15 mer.>:1 ends here

-- 11.3 Grid utilities <2021-12-15 mer.>

-- [[file:../notes.org::*11.3 Grid utilities <2021-12-15 mer.>][11.3 Grid utilities <2021-12-15 mer.>:1]]
empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (==O) ps)
    xs = length (filter (==X) ps)    
    ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g
-- 11.3 Grid utilities <2021-12-15 mer.>:1 ends here

-- 11.4 Displaying a grid <2021-12-15 mer.>

-- [[file:../notes.org::*11.4 Displaying a grid <2021-12-15 mer.>][11.4 Displaying a grid <2021-12-15 mer.>:1]]
putGrid :: Grid ->  IO ()
putGrid =
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys
-- 11.4 Displaying a grid <2021-12-15 mer.>:1 ends here

-- 11.5 Making a move

-- [[file:../notes.org::*11.5 Making a move][11.5 Making a move:1]]
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs, B:ys) = splitAt i (concat g)


chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)
-- 11.5 Making a move:1 ends here

-- 11.6 Reading a number

-- [[file:../notes.org::*11.6 Reading a number][11.6 Reading a number:1]]
getNat :: String ->  IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                       return (read xs)
                   else
                       do putStrLn "Error: Invalid number"
                          getNat prompt
-- 11.6 Reading a number:1 ends here

-- 11.7 Human vs human

-- [[file:../notes.org::*11.7 Human vs human][11.7 Human vs human:1]]
tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do cls  -- see game of life
             goto (1,1)  -- see game of life
             putGrid g
             run' g p  -- what is ' ?  juste another name

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins !\n"
         | wins X g = putStrLn "Player X wins !\n"
         | full g = putStrLn "It's a draw !\n"
         | otherwise =
             do i <- getNat (prompt p)
                case move g i p of
                  [] -> do putStrLn "Error: Invalid move"
                           run' g p
                  [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move : "
-- 11.7 Human vs human:1 ends here
