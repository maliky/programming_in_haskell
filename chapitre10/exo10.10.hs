-- Redeﬁne putStr :: String -> IO () using a list comprehension and the library function sequence_ :: [IO a] -> IO ()

putStr_ :: String -> IO ()
putStr_ [] = return ()
putStr_ (x:xs) = do putChar x
                    putStr_ xs


