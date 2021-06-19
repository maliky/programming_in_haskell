-- à partir d'une liste créer une liste de couple des éléments adjacents
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)


-- vérifie que les éléments d'une liste sont dans l'ordre croissant en vérifiant chaque paires
sorted :: Ord a => [a] ->  Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

-- renvois une liste d'indice des positions de x dans la liste xs
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

-- les diviseurs de n
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v)  <- t,  k == k']

{-
Réécrire position avec find
-}
-- renvois une liste d'indice des positions de x dans la liste xs
positions2 :: Eq a => a -> [a] -> [Int]
positions2 x xs = find x (zip xs [0..])

