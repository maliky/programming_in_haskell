{--
Using folde, deﬁne a function eval :: Expr -> Int that evaluates an ex-
pression to an integer value, and a function size :: Expr -> Int that cal-
culates the number of values in an expression.
--}

data Expr = Val Int | Add Expr Expr
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add y z) = g (folde f g y) (folde f g z)


d = Add (Val 20) (Add (Val 1) (Val 3))

eval :: Expr -> Int
eval x = folde (\x -> 1) (+) x
