-- returns all subsequences of a list
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = (subs xs) ++ map (x:) (subs xs)

-- -- returns all possible ways of inserting a new element in a list
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- -- returns all permutations of a list
perms :: [a] ->  [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- -- returns all choices from a liste
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- -- which is ...
choices_ :: [a] -> [[a]]
choices_ x = concat(map perms (subs x) )

-- exo 9.1 avec une liste compréhension composition de concat et map 
choices__ :: [a] -> [[a]]
choices__ x = [x | subs_  <- subs x, x  <- perms subs_]



