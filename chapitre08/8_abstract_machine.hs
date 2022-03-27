-- Abstract machine
data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y -- haskel choisi l'ordre d'évaluation

type Const = [Op]
data Op = EVAL Expr | ADD Int

eval :: Expr -> Const -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Const -> Int -> Int
exec []  n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n+m)

value_ :: Expr -> Int
value_ e = eval e []
