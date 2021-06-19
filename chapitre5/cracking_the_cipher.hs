import Data.Char
--import string_comprehension

-- Donne le rang d'un charactère dans l'aphabet
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- Fait l'opération inverse
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

  

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

{-
os: observered frequencies
es: expected frequencies
xsi: ième élement de la list xs
-} 

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

-- rotate la liste de n place
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- exemple
table' = freqs "kdvnhoo lv ixq"

res = [chisqr (rotate n table') table | n <- [0..25]]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']


crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs


{-
> crack "kdvnhoo lv ixq"
> crack "vscd mywzboroxcsyxc kbo ecopev"
-}
