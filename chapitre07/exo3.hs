-- exo3 redefine map f and filter p using foldr

map_ :: (a -> b) -> [a] -> [b]
map_ f xs = foldr apply_f [] xs
  where apply_f x y = (f x : y)

map__ :: (a -> b) -> [a] -> [b]
map__ f xs = foldr (\x y -> f x : y) [] xs


filter_ :: (a -> Bool) -> [a] -> [a]
filter_ p xs = foldr filter_p [] xs
  where filter_p x y
          | p x = (x:y)
          | otherwise = y
