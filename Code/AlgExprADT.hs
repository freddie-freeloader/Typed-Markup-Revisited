module AlgExprADT where

data Expr
  = Lit Int
  | Add Expr Expr

eval :: Expr -> Int
eval (Lit i)   = i
eval (Add l r) = eval l + eval r

pretty :: Expr -> String
pretty (Lit i)   = show i
pretty (Add l r) =
     "(" ++ pretty l ++ ")"
  ++ "+"
  ++ "(" ++ pretty r ++ ")"
