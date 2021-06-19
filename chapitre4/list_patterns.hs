test :: [Char] -> Bool
test ['a',_,_] = True
test _ = False

{-
la fonction test s'applique à une liste de characters (ou chaine) et renvois un Bool
Si la liste a exactement trois éléments avec le premier == à 'a' alors vrai, sinon faux.
-}


{-
: est l'opérateur de concaténation auss cons operateur

Avec cette opérateur nous définissons une fonction plus générale qui vérifie si une liste de n'importe quelle taille  commmence par 'a'
-}


test2 :: [Char] -> Bool
test2 ('a':_) = True
test2 _ = False 

head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> a
tail' (_:xs) = xs

{-
Les parenthèses sont obligatoire car sinon
head x:_ veux dire (head x):_ 
-}
