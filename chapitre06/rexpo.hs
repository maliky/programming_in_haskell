-- recursive exponentiation
rexpo :: Int -> Int -> Int
rexpo n 0 = 1
rexpo n p = n * (n `rexpo` (p - 1))
