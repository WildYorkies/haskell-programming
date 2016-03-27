-- huttonsRazor.hs 
module HuttonsRazor where

-- Your first task is to write the “eval” function which reduces an expression to a final sum.
data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add x y) = (eval x) + (eval y) 

-- Write a printer for the expressions.
printExpr :: Expr -> String 
printExpr (Lit i) = show i
printExpr (Add x y) = (printExpr x) ++ " + " ++ (printExpr y)