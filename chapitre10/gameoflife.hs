--Screen utilities
-- type Board = [Int]

-- clear the screen
cls :: IO ()
cls = putStr "\ESC[2J"


-- coordinates
type Pos = (Int, Int)

-- use control char to display a car in a given position
writeat :: Pos -> String -> IO ()
writeat pos xs = do goto pos
                    putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- The board of the game
width :: Int
width = 10

height :: Int
height = 10

-- the board is a list of (x,y)

type Board = [Pos]

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]


-- display living cell and decice if position is alive or empty

-- sequence performes a list of actions in sequence [IO a] -> IO ()
showcells :: Board -> IO ()
showcells board = sequence_ [writeat pos "0" | pos <- board]

isAlive :: Board -> Pos -> Bool
isAlive board pos = elem pos board

isEmpty :: Board -> Pos -> Bool
isEmpty board pos = not (isAlive board pos)

-- return the neighbours of a position
neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1, y-1), (x,y-1),(x+1, y-1), (x-1,y),
                          (x+1,y), (x-1, y+1),(x, y+1), (x+1,y+1)]

{-
The auxiliary function wrap takes account of the wrapping around at the edges
of the board, by subtracting one from each component of the given position,
taking the remainder when divided by the width and height of the board, and
then adding one to each component again:
-}
wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1, ((y-1) `mod` height) + 1)

-- get the number of living neigbours
liveneighbs :: Board -> Pos -> Int
liveneighbs board = length . filter (isAlive board) . neighbs

-- get the survivors, (2 or 3 neighbors)
survivors :: Board -> [Pos]
survivors board = [pos | pos <- board, elem (liveneighbs board pos) [2,3]]

-- get position to give birth  (exactly 3 neighbors)
births0 :: Board -> [Pos]
births0 board = [(x,y) | x <- [1..width], y <- [1..height],
                isEmpty board (x,y), liveneighbs board (x,y) == 3]

             
-- get position to give birth  (exactly 3 neighbors) considering only interesting cells
births :: Board -> [Pos]
births board = [pos | pos <- rmdups (concat (map neighbs board)),
                isEmpty board pos, liveneighbs board pos == 3]


-- remove duplicates
rmdups :: Eq a => [a] ->  [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

-- next generation of board
nextgen :: Board -> Board
nextgen board = survivors board ++ births board


-- finaly life
life :: Board -> IO ()
life board = do cls
                showcells board
                wait 500000
                life (nextgen board)

-- dummy action to wait
wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

                         
