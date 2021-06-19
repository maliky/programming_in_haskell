{-
Définie une fonction squre of size n
-}

grid :: Int -> Int -> [(Int,Int)]
grid n p = [(x,y) | x <- [0..n], y <- [0..p]]


square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]
