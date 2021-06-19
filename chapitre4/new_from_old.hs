-- Integral a Num type for Integers of type Integer or Int (fixed) 

even :: Integral a  => a -> Bool
-- Integral type, super classe qui inclus les Int et Integers
-- classe numérique qui doit permettre le mode et la division euclidienne

even n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

-- Fractional a Num type for Floats and Double
recip :: Fractional a => a -> a
recip x = 1 / x


-- 4.2 Conditional Expressions

abs' :: Int -> Int
abs' x =
  if x >= 0 then x else -x

  
signum' :: Int -> Int
signum' n =
  if n < 0 then -1 else
  if n == 0 then 0 else 1

-- 4.3 Guarded equations

abs'' :: Int -> Int
abs'' n
  | n >= 0 = n
  | otherwise = -n

-- | se lit "such that".  C'est plus simple à lire

signum'' :: Int -> Int
signum'' n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1


-- 4.4 Patern matching p. 56

not' :: Bool -> Bool
not False = True
not True = False

-- avec plusieurs arguments
-- and' :: Bool -> Bool -> Bool
-- True && True = True
-- True && False = False
-- False && True = False
-- False && False = False

-- nous pouvons résumer avec _
-- and' :: Bool -> Bool -> Bool
-- True && True = True
-- _ && _ = False

-- and'' :: Bool -> Bool -> Bool
-- True && b = b
-- _ && _ = False

