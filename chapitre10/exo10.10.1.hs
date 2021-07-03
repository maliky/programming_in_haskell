-- Answer
--      :PROPERTIES:
--      :header-args: :tangle chapitre10/exo10.10.1.hs :comments both :padline yes
--      :END:


-- [[file:~/Haskell/notes.org::*Answer][Answer:1]]
{-
Problem:
Redeﬁne putStr :: String -> IO () using a list comprehension and the library function sequence_ :: [IO a] -> IO ()
-}
putStr_ :: String -> IO ()
putStr_ [] = return ()
putStr_ xs = sequence_ [putChar x | x <- xs]
-- Answer:1 ends here
