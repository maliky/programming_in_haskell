-- recursive last
-- renvois le dernier élément d'une liste non vide
mylast :: [a] -> a
mylast [x] = x
mylast (x:xs) = mylast xs
