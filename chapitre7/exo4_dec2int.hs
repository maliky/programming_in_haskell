-- exo 4 p. 105
-- Using foldl, deﬁne a function dec2int :: [Int] -> Int that converts a
-- decimal number into an integer. For example:

dec2int :: [Int] -> Int
dec2int xs = foldl (\x y -> 10 * x + y) 0 xs
