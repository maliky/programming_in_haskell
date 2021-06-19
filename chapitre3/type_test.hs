{-# LANGUAGE ScopedTypeVariables #-}
-- language extension
add (x :: Int) (y :: Int) (z :: Int) = x + y + z :: Int

add2 :: Int -> Int -> Int -> Int
add2 x y z = x+y+z 

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply  f y  = f y

inverse  f x  = x f
