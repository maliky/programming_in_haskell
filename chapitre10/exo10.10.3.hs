import Chapitre10

putBoard__ :: Board -> IO ()
putBoard__ board = sequence_ [putRow n e | (n,e) <- zip [1..(length board)] board]
