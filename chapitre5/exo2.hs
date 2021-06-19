grid :: Int -> Int -> [(Int,Int)]
grid n p = [(x,y) | x <- [0..n], y <- [0..p]]
