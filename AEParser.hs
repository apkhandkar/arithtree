module AEParser (evalAExpr) where

import AExpr

evalAExpr :: (Fractional a) => AExpr a -> AExpr a
evalAExpr (Val x) = 
    Val x
evalAExpr (Para x) =
    evalAExpr x
evalAExpr (Expr o l r) =
    (\c -> case c of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> (/)) o <$> (evalAExpr l) <*> (evalAExpr r)
evalAExpr (Err s) =
    Err s
