-- mutual recursion

meven :: Int -> Bool
meven 0 = True
meven n = modd (n-1)

modd :: Int -> Bool
modd 0 = False
modd n = meven (n-1)

mevens :: [a] -> [a]
mevens [] = []
mevens (x:xs) = x : modds xs

modds :: [a] -> [a]
modds [] = []
modds (_:xs) = x : mevens xs
