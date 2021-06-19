{-
mult :: Int -> Int ->  Int -> Int
mult x y z = x*y*z
-}

mult :: Int -> Int ->  Int -> Int
mult x y z = x*y*z

-- formalisation de multiplication précédente avec la multiplication
mult2 :: Int -> Int ->  Int -> Int
mult2 = \x -> (\y -> (\z -> x*y*z))

