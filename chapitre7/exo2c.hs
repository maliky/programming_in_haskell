-- exo2c chapitre7 p. 105

takeWhile_ :: (a -> Bool) -> [a] -> [a]
takeWhile_ p (x:xs)
  | p x = [x] ++ takeWhile_ p xs
  | otherwise = []

dropWhile_ :: (a -> Bool) -> [a] -> [a]
dropWhile_ p (x:xs)
  | p x = dropWhile_ p xs
  | otherwise = (x:xs)



