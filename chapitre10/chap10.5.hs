module Chapitre10 where
-- 10.5 Derived primitives

getLine_ :: IO String
getLine_ = do x <- getChar
              if x == '\n' then
                 return []
              else
                do xs <- getLine_
                   return (x:xs)

putStr_ :: String -> IO ()
putStr_ [] = return ()
putStr_ (x:xs) = do putChar x
                    putStr_ xs

putStrLn_ :: String -> IO ()
putStrLn_ xs = do putStr_ xs
                  putChar '\n'

strlen_ :: IO ()
strlen_ = do putStr_ "Enter a string: "
             xs <- getLine_
             putStr_ "The string has "
             putStr_ (show (length xs))
             putStrLn_ " characters"
