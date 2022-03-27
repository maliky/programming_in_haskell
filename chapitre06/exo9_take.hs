-- recursive take
mytake :: Int ->  [a] -> [a]
mytake 0 xs = []
mytake _ [] = []
mytake n (x:xs) = x : take (n-1) xs

