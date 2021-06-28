myexp :: Int -> Int -> Int
myexp 0 _ = 0
myexp _ 0 = 1
myexp x y = x * myexp x (y - 1)

