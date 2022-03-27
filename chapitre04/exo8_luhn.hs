luhnDouble :: Int -> Int
luhnDouble x = if 2*x > 9 then 2*x -9 else 2*x

luhnDouble2 :: Int -> Int
luhnDouble2 x = if y > 9 then y-9 else y
  where y = 2*x

luhn :: Int -> Int -> Int ->  Int -> Bool
luhn a b c d = if mod (sum (map luhnDouble [b,c,d])) 10 == 0 then True else False 

luhn2 :: Int -> Int -> Int ->  Int -> Bool
luhn2 a b c d
  | mod (sum (map luhnDouble [b,c,d])) 10 == 0 = True
  | otherwise = False

luhn3 :: Int -> Int -> Int ->  Int -> Bool
luhn3 a b c d = mod luhnSum 10 == 0
  where
    luhnSum = sum (map luhnDouble [b,c,d])
