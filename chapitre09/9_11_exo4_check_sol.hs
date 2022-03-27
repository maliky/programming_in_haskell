data Op = Add | Sub | Mul | Div
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

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

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0


-- 9.6 Brute force solution p. 131
-- donne tout les split de la liste [a]
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls,rs) <- split xs ]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

-- returns all choices from a liste
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

choices :: [a] -> [[a]]
choices = concat . map perms . subs

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops ]


-- exprs renvois toutes les expressions dont la liste des valeurs est la liset [a]
exprs :: [Int] ->  [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
            l <- exprs ls,
            r <- exprs rs,
            e <- combine l r]

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

solutions_ :: [Int] -> Int -> [Expr]
solutions_ ns n = [e | ns_ <- choices ns, (e, m) <- results ns_, m == n]

count_exprs :: [Int] -> Int
count_exprs ns = length([e | ns_ <- choices ns, e <- exprs ns_])
  -- where a = head . reverse ns

-- solution
count_valid_sol :: Int
count_valid_sol = length([e | ns <- choices [1,3,7,10,25,50], e <- exprs ns, eval e /= []])

main :: IO ()
main = count_exprs [1,3,7,10,25,50]
