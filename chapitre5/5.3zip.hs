-- à partir d'une liste créer une liste de couple des éléments adjacents
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)


-- vérifie que les éléments d'une liste sont dans l'ordre croissant en vérifiant chaque paires
sorted :: Ord a => [a] ->  Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

-- renvois une liste d'indice des positions de x dans la liste xs
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

