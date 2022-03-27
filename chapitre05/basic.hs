[(x,y) | x <- [1,2,3], y <- [4,5]]
-- the second component change more frequently

[(x,y) | x <- [1,2,3], y <- [x..3]]

concat2 :: [[a]]  -> [a]
concat2 xss = [x | xs <- xss, x <- xs]

-- get the first element only
firsts :: [(a,b)] -> [a]
firsts ps = [x | (x,_) <- ps]

length :: [a] -> Int
length xs = sum [1 | _ <- xs]
