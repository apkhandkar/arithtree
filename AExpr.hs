module AExpr (
    AExpr (Expr, Para, Err, Val),
    Oper (Add, Mul, Sub, Div)
    ) where

data Oper = Add | Mul | Sub | Div deriving (Show, Eq)

data AExpr a =  
    Expr Oper (AExpr a) (AExpr a)
    | Para (AExpr a) 
    | Err String
    | Val a deriving (Show, Eq)

instance Functor AExpr where
    fmap g (Val a)      = Val (g a)
    fmap g (Para x)     = Para (fmap g x)
    fmap g (Expr o l r) = Expr o (fmap g l) (fmap g r)
    fmap g (Err s)      = Err s

instance Applicative AExpr where
    pure                        = Val
    (Val f) <*> (Val x)         = Val (f x)
    (Val f) <*> (Para x)        = Para ((Val f) <*> x)
    (Val f) <*> (Expr o l r)    = Expr o ((Val f) <*> l) ((Val f) <*> r) 
    f       <*> (Val x)         = pure ($ x) <*> f
    _       <*> _               = Err "AExpr tree mismatch"
