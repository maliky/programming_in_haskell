module Expressions where
import DataTypes

-- expresion to use
e1 :: Expr
e1 = (App Add (Val 2) (Val 3))    

e2 :: Expr  -- invalide
e2 = (App Sub (Val 2) (Val 3))    

e3 :: Expr  -- for 9.5
e3 = (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10)))


e4 :: Expr  -- for 9.6
e4 = (App Exp (Val 2) (Val 3))
