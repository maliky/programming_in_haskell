module Exo10 where
{-
10. Using altMap, deﬁne a function luhn :: [Int] -> Bool that implements
the Luhn algorithm from the exercises in chapter 4 for bank card numbers of
any length. Test your new function using your own bank card.
-}

altMap :: (a -> b) ->  (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = (f x : altMap g f xs)

{-----
8. The Luhn algorithm is used to check bank card numbers for simple errors
such as mistyping a digit, and proceeds as follows:
- consider each digit as a separate number;
- moving left, double every other number from the second last;
- subtract 9 from each number that is now greater than 9;
- add all the resulting numbers together;
- if the total is divisible by 10, the card number is valid.
-----}

-- luhn_ xs =  if sum_luhn xs `mod` 10 == 0 then True else False where
--   sum_luhn (x:xs) =  sum (altMap (sub_9) (sub_9) xs) + x where
--     sub_9 x = if double_x > 10 then double_x - 9 else double_x where
--       double_x = 2 * x
luhn_ :: [Int] -> Bool
luhn_ (x:xs) =  if mod (sum_luhn xs) 10 == 0 then True else False where
  sum_luhn xs =  sum (altMap (sub_9) (sub_9) xs) where
    sub_9 x = if 2*x > 9 then 2*x - 9 else 2*x where

-- luhn__ :: [Int] -> [Int]
luhn__ (x:xs) =  altMap (sub_9) (sub_9) xs ++ [sum_luhn xs] where
  sub_9 x = if 2*x > 9 then 2*x - 9 else 2*x
  sum_luhn xs =  sum (altMap (sub_9) (sub_9) xs) 

      

