import System.IO
import Chapitre10

-- IO Char est un diminutif de World -> (Char, World)
-- IO Char is the action returning a Char
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x
            
sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
              else
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)


hangman_ :: IO ()
hangman_ = do putStrLn_ "Think of a word:"
              word <- sgetLine
              putStrLn_ "Try to guess it:"
              play word

play :: String -> IO ()
play word = do putStr_ " ? "
               guess <- getLine
               if guess == word then
                 putStrLn_ "You got it ! "
               else
                 do putStrLn_ (match word guess)
                    play word


match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]
