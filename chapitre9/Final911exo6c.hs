module Final911exo6 where

import Data.List
import DataTypes
import Expressions

-- Define the operators
ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

-- valid takes an operator and 2 Int and return if the operation is valid
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = x /= 1 && y /= 1 && (x `mod` y == 0)
valid Exp x y = y > 1 && x /= 1

-- apply an operator to 2 Int
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = myexp x y

-- define an exponential working on Int
myexp :: Int -> Int -> Int
myexp 0 _ = 0
myexp _ 0 = 1
myexp x y = x * myexp x (y - 1)

-- Return the Int used in a Expression
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

-- compute the Expression value
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- returns all subsequences of a list
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x :) yss
  where
    yss = subs xs

-- returns all possible ways of inserting a new element in a list
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x : y : ys) : map (y :) (interleave x ys)

-- returns all permutations of a list
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- returns all choices from a liste
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- 9.5 check if we can make a solution from the list of numbers
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- elem check if values e, si les valeurs de l'expression e (dans l'ordre) apparaissent dans l'ensemble des choix possibles consruis à partir de ns.

-- 9.6 Brute force solution p. 131
-- donne tout les split de la liste [a]
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

-- exprs renvois toutes les expressions dont la liste des valeurs est la liste [a]
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =
  [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

-- Créer all possible expression for operators and Expression
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

-- Renvois toutes les expressions construites à partir de la liste des Int et dont la valeur est n.
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns_ <- choices ns, e <- exprs ns_, eval e == [n]]

-- Crée et évalue les expressions valides construitent à partir de la liste d'entiers
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns =
  [ res
  | (ls, rs) <- split ns
  , lx <- results ls
  , ry <- results rs
  , res <- combine_ lx ry
  ]

results_complex :: [Int] -> [Result_complex]
results_complex [] = []
results_complex [n] = [(Val n, n, 0) | n > 0]
results_complex ns =
  [ (res, head (complexity res))  
  | (ls, rs) <- split ns
  , lx <- results ls
  , ry <- results rs
  , res <- combine_ lx ry
  ]

-- évalue la complexité d'un Résultat
complexity :: [Result] -> [Int]
complexity [] = [0.0]
complexity [(e,i)] = [complexity_ e]
complexity (r:rs) = complexity [r] ++ complexity rs


complexity_ :: Expr -> Int
complexity_ (Val n) = 0
complexity_ (App Add l r) = 1 + complexity_ l + complexity_ r
complexity_ (App Sub l r) = 2 + complexity_ l + complexity_ r
complexity_ (App Mul l r) = 3 + complexity_ l + complexity_ r
complexity_ (App Div l r) = 4 + complexity_ l + complexity_ r
complexity_ (App Exp l r) = 5 + complexity_ l + complexity_ r

-- 
combine_ :: Result -> Result -> [Result]
combine_ (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

-- 
solutions_ :: [Int] -> Int -> [Expr]
solutions_ ns n = [e | ns_ <- choices ns, (e, m) <- results ns_, m == n]

-- soit un nombre n, et deux tuple, je compare la différence des deuxième argument des tuples avec n
-- utile pour un sort
compare_second_arg_diff :: Int -> (a, Int) -> (a, Int) -> Ordering
compare_second_arg_diff n (x1, x2) (y1, y2) =
  compare (abs (x2 - n)) (abs (y2 - n))

-- je renvois le deuxième argument d'un tuple
get_second_tuple_arg :: (Expr, Int) -> Int
get_second_tuple_arg (x, y) = y

-- récupère les expressions permettant d'obtenir le nombre n ou le nombre qui lui et le plus proche
solutions_proches :: [Int] -> Int -> ([Expr], Int)
solutions_proches ns n =
  ( [sol_exp | (sol_exp, sol_num) <- sols, sol_num == best_estimate]
  , best_estimate)
  where
    sols = [(e, m) | ns_ <- choices ns, (e, m) <- results ns_]
    diffs = sortBy (compare_second_arg_diff n) sols
    best_estimate = get_second_tuple_arg (head diffs)

solutions_simple :: [Int] -> Int -> [(Expr, Int)]
solutions_simple ns n = sols
  where
    sols = [(e, m) | ns_ <- choices ns, (e, m) <- results ns_]
    diffs = sortBy (compare_second_arg_diff n) sols



