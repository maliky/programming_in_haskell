rmult :: Int -> Int -> Int
m `rmult` 0 = 0
m `rmult` n = m + (m * (n-1))

