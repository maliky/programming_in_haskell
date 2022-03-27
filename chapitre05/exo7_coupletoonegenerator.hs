{-
-- Refaire avec seulement un générateur:
[(x,y) | x <- [1,2], y <- [3,4]]
-}

concat [[(x,y) | x <- [1,2]] | y <- [3,4]]
