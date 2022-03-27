-- merge and sort
msort ::  Ord a =>  [a] -> [a]
msort [] = []
msort [x] = [x]
msort (x:xs) = msort half1 ++ msort half2
  where
    lesser

