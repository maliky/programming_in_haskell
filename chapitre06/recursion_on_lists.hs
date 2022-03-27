--recursion on lists
product' :: Num a  => [a] -> a
product' [] = 1
product' (n:ns) = n * product ns

length' :: [a] -> Int
length' = 0
length (_:xs) = 1 + length xs

reverse :: [a] -> [a]
reverse [] =  []
reverse (x:xs) = reverse xs ++ [x]

-- inserer un element dans une liste déjà triée
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys


-- isort insert et sort a liste
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)
