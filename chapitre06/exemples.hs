  -- show how length, drop and init are evaluated

  length :: [a]
  length [] = 0
  length (_:xs) = 1 + length xs

  -- drop a number of elements 
  drop :: Int -> [a] -> [a]
  drop 0 xs = xs
  drop _ [] = []
  drop n (_:xs) = drop (n-1) xs

  -- remove the last element
  init :: [a] -> [a]
  init [_] = []
  init (x:xs) = x : init xs
