module AlgExprBB where

data ExprBB a = ExprBB { lit :: Int -> a
                       , add :: a -> a -> a }

exprBP :: ExprBB a -> a
exprBP (ExprBB lit add) = add (lit 4) (lit 2)

evalExprBB :: ExprBB Int
evalExprBB = ExprBB evalInt evalAdd
  where
    evalInt i   = i
    evalAdd l r = l + r

prettyExprBB :: ExprBB String
prettyExprBB = ExprBB evalInt evalAdd
 where
  evalInt     = show
  evalAdd l r =
       "(" ++ l ++ ")"
    ++ "+"
    ++ "(" ++ r ++ ")"

evalExprBB' :: ExprBB Int
evalExprBB' = ExprBB id (+)

data NegBB a = NegBB { neg :: a -> a }

evalNegBB :: NegBB Int
evalNegBB = NegBB (\e -> -e)

prettyNegBB :: NegBB String
prettyNegBB = NegBB (\e -> "-" ++ e)

mixedExpr :: ExprBB a -> NegBB a -> a
mixedExpr (ExprBB lit add) (NegBB neg) = add (lit 4) (neg (lit 2))
