module Final911exo6 where

import Data.List
import DataTypes
import Expressions
import MlkUtils

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


-- 9.5 check if we can make a solution from the list of numbers
-- solution :: Expr -> [Int] -> Int -> Bool
-- solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- elem check if values e, si les valeurs de l'expression e (dans l'ordre) apparaissent dans l'ensemble des choix possibles consruis à partir de ns.

-- Create all possible expressions from the 2 other expressions
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

-- Return all valide Result_complexs built from 2 sub results
combine_ :: Result_complex -> Result_complex -> [Result_complex]
combine_ (l, x, a) (r, y, b) = [(App o l r, apply o x y, complexity_ l + complexity_ r) | o <- ops, valid o x y]


-- Return all possible expressions whose values is the given list of Int
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =
  [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]


-- Return valide expressions from the list of Int with their results
results :: [Int] -> [Result_complex]
results [] = []
results [n] = [(Val n, n, 0) | n > 0]
results ns =
  [ res
  | (ls, rs) <- split ns
  , lx <- results ls
  , ry <- results rs
  , res <- combine_ lx ry
  ]

-- évalue la complexité d'un Résultat
complexity_ :: Expr -> Int
complexity_ (Val n) = 0
complexity_ (App Add l r) = 1 + complexity_ l + complexity_ r
complexity_ (App Sub l r) = 2 + complexity_ l + complexity_ r
complexity_ (App Mul l r) = 3 + complexity_ l + complexity_ r
complexity_ (App Div l r) = 4 + complexity_ l + complexity_ r
complexity_ (App Exp l r) = 5 + complexity_ l + complexity_ r

-- 
solutions_ :: [Int] -> Int -> [(Expr, Int, Int)]
solutions_ ns n = [(e, m, x) | ns_ <- choices ns, (e, m, x) <- results ns_, m == n]

-- -- je renvois le deuxième argument d'un tuple
-- get_second_tuple_arg :: (Expr, Int) -> Int
-- get_second_tuple_arg (x, y) = y

-- récupère les expressions permettant d'obtenir le nombre n ou le nombre qui lui et le plus proche
-- solutions_proches :: [Int] -> Int -> ([Expr], Int)
-- solutions_proches ns n =
--   ( [sol_exp | (sol_exp, sol_num, sol_comp) <- sols, sol_num == best_estimate]
--   , best_estimate)
--   where
--     sols = [(e, m, x) | ns_ <- choices ns, (e, m, x) <- results ns_]
--     diffs = sortBy (compare_second_arg_diff n) sols
--     best_estimate = get_second_tuple_arg (head diffs)

-- return the simplest solution first, showing the measure
solutions_simple_ :: [Int] -> Int -> [(Expr, Int, Int)]
solutions_simple_ ns n = sorted_sols
  where
    sols = [(e, m, x) | ns_ <- choices ns, (e, m, x) <- results ns_, m == n]
    sorted_sols = sortBy (\(_,_,a) (_,_,b) -> compare a b) sols

-- return the simplest solution first hiding measures
solutions_simple :: [Int] -> Int -> [Expr]
solutions_simple ns n = [a | (a,b,c)  <- sorted_sols]
  where
    sols = [(e, m, x)| ns_ <- choices ns, (e, m, x) <- results ns_, m == n]
    sorted_sols = sortBy (\(_,_,a) (_,_,b) -> compare a b) sols


