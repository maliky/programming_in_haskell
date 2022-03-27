-- réplique l'élément a n fois
replicate2 :: Int -> a -> [a]
replicate2 n x = [x | _ <- [0..n]]
