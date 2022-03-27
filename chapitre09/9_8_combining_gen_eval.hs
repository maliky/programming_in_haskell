data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y


data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- expresion to use
e1 :: Expr
e1 = (App Add (Val 2) (Val 3))    

e2 :: Expr  -- invalide
e2 = (App Sub (Val 2) (Val 3))    

e3 :: Expr  -- for 9.5
e3 = (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10)))

-- returns all subsequences of a list
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

-- returns all possible ways of inserting a new element in a list
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- returns all permutations of a list
perms :: [a] ->  [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- returns all choices from a liste
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- which is also ...
choices_ :: [a] -> [[a]]
choices_ x = concat(map perms (subs x) )


-- 9.5 check if we can make a solution from the list of numbers
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]
-- elem check if values e, si les valeures de l'expression e (dans l'ordre) apparaissance dans l'ensemble des choix possibles consruire à partir de ns.


-- 9.6 Brute force solution p. 131
-- donne tout les split de la liste [a]
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls,rs) <- split xs ]

-- exprs renvois toutes les expressions dont la liste des valeurs est la liset [a]
exprs :: [Int] ->  [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
            l <- exprs ls,
            r <- exprs rs,
            e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops ]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns_ <- choices ns, e <- exprs ns_, eval e == [n]]

type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
               lx <- results ls,
               ry <- results rs,
               res <- combine_ lx ry]

combine_ :: Result -> Result -> [Result]
combine_ (l, x) (r, y) =  [(App o l r, apply o x y) | o <- ops, valid o x y]

soluions_ :: [Int] -> Int -> [Expr]
solutions_ ns n = [e | ns_ <- choices ns, (e, m) <- results ns_, m == n]
