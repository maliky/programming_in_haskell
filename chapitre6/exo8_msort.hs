-- merge two list
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x > y = (y: merge (x:xs) ys)
                    | otherwise = (x: merge xs (y:ys))

halving ::  [a] -> ([a],[a])
halving xs = splitAt n xs
  where n = (length xs `div` 2 )

-- merge sort
msort :: Ord a => [a] ->  [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort h1) (msort h2)
  where (h1,h2) = halving xs 
