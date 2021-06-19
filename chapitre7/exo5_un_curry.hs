{-
-- exo 5 p. 106 --
Without looking at the deﬁnitions from the standard prelude, deﬁne the higher-order library function curry that converts a function on pairs into a curried function, and, conversely, the function uncurry that converts a curried function with two arguments into a function on pairs.
-}

curry_ :: ((a,b) -> c) -> (a -> b -> c)
curry_ f = \x y -> f (x,y)

uncurry_ :: (a -> b -> c)  -> (a,b) -> c
uncurry_ f = \(x,y) -> f x y

sum_ :: Num a => (a,a) -> a
sum_ (x,y) = x + y

sum__ :: Num a => a -> a -> a
sum__ x y = x + y
