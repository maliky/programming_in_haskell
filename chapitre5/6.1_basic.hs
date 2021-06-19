(mymult) :: Int -> Int -> Int
m mymult 0 = 0
m mymult n = m + (m mymult (n-1))
