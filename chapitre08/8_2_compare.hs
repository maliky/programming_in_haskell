{--
Although not included in appendix B, the standard prelude deﬁnes
data Ordering = LT | EQ | GT
together with a function
compare :: Ord a => a -> a -> Ordering
that decides if one value in an ordered type is less than (LT), equal to (EQ),
or greater than (GT) another value. Using this function, redeﬁne the function
occurs :: Ord a => a -> Tree a -> Bool for search trees. Why is this
new deﬁnition more eﬃcient than the original version?
--}

-- data Ordering_ = LT | EQ | GT deriving Show
-- compare_ :: Ord a => a -> a -> Ordering
-- compare_ x y | x == y = EQ
--             | x < y = LT
--             | otherwise = GT
            
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord a  => a -> Tree a -> Bool
occurs x (Leaf y) = compare x y == EQ
occurs x (Node l y r) | compare x y == EQ = True
                       | compare x y == LT = occurs x l
                       | otherwise = occurs x r


-- original
occurs_ :: Ord a => a -> Tree a -> Bool
occurs_ x (Leaf y) = x == y
occurs_ x (Node l y r) | x == y = True
                       | x < y = occurs_ x l
                       | otherwise = occurs_ x r



-- Voir la réponse

