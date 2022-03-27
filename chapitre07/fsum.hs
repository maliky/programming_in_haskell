-- my fsum.hs file

fsum :: Num a => [a] -> a
fsum = foldr (+) 0

snoc x xs = xs ++ [x]

myreverse [] = []
myreverse (x:xs) = snoc x (reverse xs)
