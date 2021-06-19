{--
Consider the following type of binary trees:
data Tree a = Leaf a | Node (Tree a) (Tree a)
Let us say that such a tree is balanced if the number of leaves in the left and
right subtree of every node diﬀers by at most one, with leaves themselves be-
ing trivially balanced. Deﬁne a function balanced :: Tree a -> Bool that
decides if a binary tree is balanced or not.
Hint: ﬁrst deﬁne a function that returns the number of leaves in a tree.
--}

data Tree a = Leaf a | Node (Tree a) (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 6) (Leaf 9))

t2 :: Tree Int
t2 = Node (Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 5) (Leaf 2))) (Node (Leaf 6) (Leaf 9))

count_leaves :: Tree a -> Int
count_leaves (Leaf a) = 1
count_leaves (Node l r) = count_leaves l + count_leaves r


balanced :: Tree a -> Bool 
balanced (Node l r) | abs (count_leaves l - count_leaves r) < 2 = True
                  | otherwise = False
