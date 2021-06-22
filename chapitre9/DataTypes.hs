module DataTypes where

-- What is an Operator
data Op = Add | Sub | Mul | Div | Exp

-- What is an Expression
data Expr = Val Int | App Op Expr Expr

-- the Result type
type Result = (Expr, Int)

-- A Result and complexity
type Result_complex = (Expr, Int, Int)

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
  
