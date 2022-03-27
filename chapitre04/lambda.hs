-- est la fonction sans nom qui prend un x et le double
-- \ représente lambda
-- taper> (\x -> x + x) 2
add2 :: Int -> (Int ->  Int)
add2 = \x -> (\y -> x + y)


const' :: a -> b -> a
const' x _ = x

-- fonction constante, pas très claire, alors que comme suis plus net

const2 :: a -> b -> a
const2 x = \_ -> x

-- renvois une fonction constante

odds2 :: Int -> [Int]
odds2 n =
  map f [0..n-1]
  where f x = x*2 + 1

-- et maintenant avec lambda
odds3 :: Int -> [Int]
odds3 n = map (\x -> x*2 +1)  [0..n-1]

-- nous appliquons la fonction sur la iste.

