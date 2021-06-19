-- select the nth element of a list (!!)
mysel :: [a] -> Int -> a
mysel (x:xs) 0 = x
mysel (x:[]) _ = x
mysel (x:xs) n = mysel xs (n-1)


