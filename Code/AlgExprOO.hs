module AlgExprOo where

data ExprOO = ExprOO { evalThis   :: Int
                     , prettyThis :: String}

newLit :: Int -> ExprOO
newLit i = ExprOO i (show i)

newAdd :: ExprOO -> ExprOO -> ExprOO
newAdd l r = ExprOO evalResult prettyResult
 where
  evalResult   = evalThis l + evalThis r
  prettyResult =
       "(" ++ prettyThis l ++ ")"
    ++ "+"
    ++ "(" ++ prettyThis r ++ ")"

newNeg :: ExprOO -> ExprOO
newNeg e = ExprOO (- evalThis e) ("- " ++ prettyThis e)

exOO :: ExprOO
exOO = newAdd (newLit 4) (newNeg (newLit 2))
