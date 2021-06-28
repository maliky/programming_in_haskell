import Data.Char -- for digitToInt
-- Nim game Two players then take it in turn to remove one or more stars from the end of a single row. The winner is the player who makes the board empty, that is, who removes the ﬁnal star or stars from the board. To contrast with the top-down development of the hangman game in the previous section, we implement nim in a bottom-up manner, starting by deﬁning a series of utility functions, which are then used to implement the game itself.

-- Utilities functions

-- give the next player
next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

-- number of stars in the initial Board
initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
  where update r n = if r == row then n-num else n


-- IO Utilities

-- display the row of a board on the screen given row num and stars remaining
putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))


putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                       return (digitToInt x)
                     else
                       do putStrLn "Error : Invalid digit"
                          getDigit prompt

                  
newline :: IO ()
newline = putChar '\n'


-- Main game loop

play :: Board -> Int -> IO ()
play board player =
     do newline
        putBoard board
        if finished board then
           do newline
              putStr "Player "
              putStr (show (next player))
              putStrLn " wins!!"
        else
          do newline
             putStr "Player "
             putStrLn (show player)
             row <- getDigit "Enter a row number : "
             num <- getDigit "Stars to remove : "
             if valid board row num then
                play (move board row num) (next player)
             else
                do newline
                   putStrLn "Error : Invalid move"
                   play board player

nim :: IO ()
nim = play initial 1
     
