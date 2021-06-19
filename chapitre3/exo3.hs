-- type of 
second xs = head (tail xs)
-- [a] -> [a] -> a
-- [a] -> a

swap (x,y) = (y,x)
-- (a,b) -> (b,a)

pair x y = (x,y)
-- a -> b -> (a,b)

double x = x*2
-- Num => a -> a -> a
-- Num => a -> a

palindrome xs = reverse xs == xs
-- Eq => [a] -> [a] -> [a] -> Bool
-- Eq a => [a] -> Bool

twice f x = f (f x)
-- a -> a
-- (t -> t) -> t -> t



