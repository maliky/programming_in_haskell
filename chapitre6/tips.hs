-- product as recurcive
product :: Num a => [a] -> [a]
product = foldr (*) 1  -- I don't get this. see chapter 7

-- not the difference foldr foldl


drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs


init :: [a] -> [a]
init [_] = []
init (x:xs) = x : init xs
