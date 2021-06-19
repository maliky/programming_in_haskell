-- exo2 chapitre7 p. 105

all_ :: (a -> Bool) -> [a] -> Bool
all_ p xs = foldr test_and True xs
  where test_and x b = p x && b

any_ :: (a -> Bool) -> [a] -> Bool
any_ p xs = foldr test_or False xs
  where test_or x b = p x || b
