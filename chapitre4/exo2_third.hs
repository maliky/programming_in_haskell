{-
Définir la fonction third qui renvoie le 3ème élément d'une liste qui en contient suffisament, en utilisant
head and tail
list indexing !!
patter matching
-}

third :: [a] -> a
third xs =  head (tail (tail xs))

third2 :: [a] -> a
third2 xs =  xs !! 2

third3 :: [a] -> a
third3 (_ : ( _ : (x : _))) = x

-- third4 (_ : _ : x : _) = x
