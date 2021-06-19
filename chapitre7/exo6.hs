module Exo6 where
{-
A higher-order function unfold that encapsulates a simple pattern of recursion for producing a list can be deﬁned as follows:
unfold p h t x | p x= []
| otherwise = h x : unfold p h t (t x)
That is, the function unfold p h t produces the empty list if the predicate p is true of the argument value, and otherwise produces a non-empty list by applying the function h to this value to give the head, and the function t to generate another argument that is recursively processed in the same way to produce the tail of the list. For example, the function int2bin can be rewritten more compactly using unfold as follows:

int2bin = unfold (== 0) (‘mod‘ 2) (‘div‘ 2)

Redeﬁne the functions chop8, map f and iterate f using unfold
-}

-- unfold :: (a -> Bool) -> (a -> a) -> (a -> a) -> a -> [a]
-- why no type definission here?
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2bin_ :: Int -> [Int]
int2bin_ = unfold (== 0) (`mod` 2) (`div` 2)

-- chop8_
chop8_ :: [a] -> [[a]]
chop8_ = unfold (null) (take 8) (drop 8)

-- map f
map_ :: (a -> b) -> [a] -> [b]
map_ f xs = unfold (null) (f . head) (tail) xs

-- iterate f
iterate_ :: Num a =>  Eq a =>  (a -> a) -> a -> [a]
iterate_ f x = unfold (== 0) (id) (f) (x)


