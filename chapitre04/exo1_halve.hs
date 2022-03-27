{-
4.8 Exercices
exo 1 p. 61
Using library functions, define a function havle that splist an even-lengthed list into halves.
-}

-- Proposition 1, works but does not look very nice
halve :: [a] -> ([a],[a])
halve xs 
  | even (length xs) = (take (length xs `div` 2) xs, take (length xs `div` 2) (reverse xs))
  | otherwise = (xs,[])


-- proposition 2, trying to factor the code but ...
halve2 :: [a] -> ([a],[a])
halve2 xs 
  | even n = (take k xs, take k (reverse xs))
  | otherwise = (xs,[])
  where
    n = length xs
    k = div n 2

{-
-- error: Not in scope: data constructor ‘N’
-- error: Not in scope: data constructor ‘K’

Solved! don't use capital letter for variables

-}

-- proposition 3, trying to factor even more
halve3 :: [a] -> ([a],[a])
halve3 xs 
  | even n = (half xs, half (reverse xs))
  | otherwise = (xs,[])
  where
    n = length xs
    k = div n 2
    half = take k
 

-- proposition 4, using more library function and correcting reverse bug
halve4 :: [a] -> ([a],[a])
halve4 xs 
  | even n = splitInHalf xs
  | otherwise = (xs,[])
  where
    n = length xs
    splitInHalf xs = (take k xs, drop k xs)
      where
        k = div n 2



