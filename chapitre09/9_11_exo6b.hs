numbers :: [Int]
numbers = [1,3,7,10,25,50]


count_valid_sol :: Int
count_valid_sol = length([e | ns <- choices numbers, e <- exprs ns, eval e /= []])
