{-# LANGUAGE MultiWayIf #-}
import System.IO
-- import Chapitre10
{-
exercice : 10.10.6 p. 138 Hutton_G_Programming_in_Haskell-Cambridge_University_Press_2018.pdf
Using getCh, deﬁne an action readLine :: IO String that behaves in the same way as getLine,  except that it also permits the delete key to be used to remove characters. 

Hint: the delete character is ’\DEL’, and the control character for moving the cursor back one space is ’\b’.
-}

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine_ :: String  -> IO String
readLine_ full_ = do next_char<- getCh
                     if | next_char== '\n' -> return full_
                        | next_char== '\DEL' -> do
                            sequence_ (map putChar "\b \b")
                            readLine_ reduced_ 
                        | otherwise -> do
                            putChar next_char
                            readLine_ (full_ ++ [next_char])
                     where
                       reduced_ = take (length full_ -1) full_


readLine :: IO String
readLine = readLine_ "" ""
