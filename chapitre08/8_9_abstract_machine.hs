-- Abstract machine
data Expr = Val Int | Add Expr Expr | Mul Expr Expr  -- adding mul

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y -- haskel choisi l'ordre d'évaluation
value (Mul x y) = value x * value y

type Const = [Op]
data Op = EVAL Expr | ADD Int | MUL Int  -- adding mul

eval :: Expr -> Const -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)
eval (Mul x y) c = eval x (EVAL y : c)  -- adding mul

exec :: Const -> Int -> Int
exec []  n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n+m)
exec (MUL n : c) m = exec c (n * m)   -- adding mul

value_ :: Expr -> Int
value_ e = eval e []
