{-
un nombre est parfait si il est égale à la somme de ses facteurs à l'exclusion de lui même
-}

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (factors x) - x ]
