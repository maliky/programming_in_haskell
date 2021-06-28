module Chapitre10 where
-- 10.5 Derived primitives

-- Return a String value
getLine_ :: IO String
getLine_ = do x <- getChar
              if x == '\n' then
                 return []
              else
                do xs <- getLine_
                   return (x:xs)

-- Read Take a String echo it and return empty
putStr_ :: String -> IO ()
putStr_ [] = return ()
putStr_ (x:xs) = do putChar x
                    putStr_ xs

-- Read Take a String echo (with empty line) it and return emptyness
putStrLn_ :: String -> IO ()
putStrLn_ xs = do putStr_ xs
                  putChar '\n'

-- Ask for a string, get it and echo its length
strlen_ :: IO ()
strlen_ = do putStr_ "Enter a string: "
             xs <- getLine_
             putStr_ "The string has "
             putStr_ (show (length xs))
             putStrLn_ " characters"
