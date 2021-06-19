data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add_ :: Nat -> Nat -> Nat
add_ m n = int2nat (nat2int m + nat2int n)


-- sans le conversion

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)


-- 
data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten 1 ++ [x] ++ flatten r

-- for search tree (ordred tree)
occurs_ :: Ord a => a -> Tree a -> Bool
occurs_ x (Leaf y) = x == y
occurs_ x (Node l y r) | x == y = True
                       | x < y = occurs x l
                       | otherwise = occurs x r


-- d'autre type d'arbres
data Tree a = Leaf a | Node (Tree a) (Tree a)
data Tree a = Leaf  | Node (Tree a) a (Tree a)
data Tree a b = Leaf a | Node (Tree a b) b (Tree a b)
data Tree a = Node a [Tree a]
