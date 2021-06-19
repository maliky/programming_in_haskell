{--
Deﬁne a function balance :: [a] -> Tree a that converts a non-empty
list into a balanced tree. Hint: ﬁrst deﬁne a function that splits a list into two
halves whose length diﬀers by at most one.
--}

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show  

t :: Tree Int
t = Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 6) (Leaf 9))

t2 :: Tree Int
t2 = Node (Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 5) (Leaf 2))) (Node (Leaf 6) (Leaf 9))

split_balance :: [a] -> ([a],[a])
split_balance [] = ([],[])
split_balance xs
  | even n = (take p xs, drop p xs)
  | otherwise = (take k xs, drop k xs)
  where
    n = length xs
    p = n `div` 2
    k = p + n `mod` 2

balance :: [a] -> Tree a
balance (x:[]) = (Leaf x)
balance xs | length xs > 1 = (Node (balance l) (balance r))
  where
    (l, r) = split_balance xs

