{-
from operator to curried function
() pour passer de l'operateur à la curried function
(+) addition
(1+) la fonction successeur
(1/) la fonction inverse
(*2) la fonction double
(/2) la fonction moitié
-}


-- operateur comme argument

sum' :: [Int] -> Int
sum' = foldl (+) 0
