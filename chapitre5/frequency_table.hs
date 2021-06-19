-- Renvois le nombre de lettre lowers dans le string 
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z' ]

-- Renvois le nombre d'occurence du char x dans la chaine xs
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']


{-
Frenquency tables of letters in english
-}
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,6.3, 9.0, 2.8, 1.0, 2.4,0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- fromIntegral converti un Int in Float

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n_lower | x <- ['a'..'z']]
  where n_lower = lowers xs
