{-
generate the pythagorean triplets
-}

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..x], z <- [1..y], x^2 == y^2+ z^2 ]
