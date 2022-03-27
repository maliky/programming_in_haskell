-- decide if the element is in the list
elinl :: Eq a => a -> [a] -> Bool
elinl _ [] = False
elinl y (x:xs) | y == x = True
               | otherwise = elinl y xs
