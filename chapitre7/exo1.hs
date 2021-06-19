-- [f x | x <- xs, p x] ?
exo1 :: (a -> b) -> [a] -> (a -> Bool) -> [b]
exo1 f xs p = [f x | x <- xs, p x]

exo1sol :: (a -> b) -> [a] -> (a -> Bool) -> [b]
exo1sol f xs p = map f (filter p xs)
-- map f filter p xs
