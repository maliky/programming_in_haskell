module MlkUtils where

-- returns all sublist of a list
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x :) yss
  where
    yss = subs xs

-- returns all possible ways of inserting a new element in a list
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x : y : ys) : map (y :) (interleave x ys)

-- returns all permutations of a list
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- returns all choices from a list
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- guive all possible splits of a list of a
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]


-- Soit un nombre n et deux tuples, je compare la différence des deuxième argument des tuples avec n
-- utile pour un sort
compare_second_arg_diff :: Int -> (a, Int) -> (a, Int) -> Ordering
compare_second_arg_diff n (x1, x2) (y1, y2) =  compare (abs (x2 - n)) (abs (y2 - n))
