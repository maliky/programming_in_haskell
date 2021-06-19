-- Renvois le nombre de lettre lowers dans le string 
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z' ]

-- Renvois le nombre d'occurence du char x dans la chaine xs
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

