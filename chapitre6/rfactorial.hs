rfac :: Int -> Int
rfac 0 = 1
rfac n | n >= 1 = n * rfac (n-1)
       | otherwise = 0

