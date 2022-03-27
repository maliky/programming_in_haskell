import Chapitre10

putBoard_aux :: Board -> Int -> IO ()
putBoard_aux board 1 = putRow 1 (board !! 0)
putBoard_aux board n = do putBoard_aux board (n-1)
                          putRow n (board !! (n-1))


putBoard_ :: Board -> IO ()
putBoard_ board = putBoard_aux board (length board)
