{--
Given the type declaration
data Expr = Val Int | Add Expr Expr
deﬁne a higher-order function
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
such that folde f g replaces each Val constructor in an expression by the
function f, and each Add constructor by the function g .
--}

data Expr = Val Int | Add Expr Expr
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add y z) = g (folde f g y) (folde f g z)
