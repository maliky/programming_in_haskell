module DataTypes where

data Op = Add | Sub | Mul | Div | Exp

data Expr = Val Int | App Op Expr Expr

type Result = (Expr,Int)

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
  show Exp = "**"
  
