-- 2. Deﬁne a recursive function isChoice :: Eq a => [a] -> [a] -> Bool that decides if one list is chosen from another, without using the combinatorial functions perms and subs. Hint: start by deﬁning a function that removes the ﬁrst occurrence of a value from a list.
-- isChoice :: Eq a => [a] -> [a] -> Bool

-- removefirst :: Eq a => [a] -> a

-- ischoice [2,1]  [1,2,3] -> True

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] [] = True
isChoice [x] ys = elem x ys
isChoice (x:xs) ys = elem x ys && isChoice xs ys


