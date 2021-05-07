module AEZipper (
    Crumb (LExpr, RExpr, DPara),
    AEZipper,
    zipper,
    snag,
    zipLeft,
    zipRight,
    zipDown,
    zipUp,
    zipTop,
    zInsert,
    compose,
    (-:),
    (.>)
    ) where

import AExpr

data Crumb a =
    LExpr Oper (AExpr a) |
    RExpr Oper (AExpr a) |
    DPara deriving (Show, Eq)

data AEZipper a = Zipper ((AExpr a), [Crumb a]) | Snag deriving (Show, Eq)

instance Functor Crumb where
    fmap f (LExpr o e) = LExpr o (f <$> e)
    fmap f (RExpr o e) = RExpr o (f <$> e)
    fmap f (DPara) = DPara

instance Functor AEZipper where
    fmap f (Zipper (e, cs)) = Zipper (f <$> e, (f <$>) <$> cs)
    fmap f Snag             = Snag

zipper :: (AExpr a) -> AEZipper a
zipper e = Zipper (e, [])

snag :: AEZipper a
snag = Snag

-- Make composing Zipper functions look neater

(-:) :: AEZipper a -> (AEZipper a -> AEZipper b) -> AEZipper b
zipper -: fs = fs zipper

-- Build a composite function from a list

compose [] = id
compose (f:fs) = f .> (compose fs)

-- Left-associative composition operator

(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g.f

-- Move left, right, down, up and to the top of the tree

zipLeft :: AEZipper a -> AEZipper a
zipLeft Snag = Snag
zipLeft (Zipper (Expr o l r, bs)) = Zipper (l, (LExpr o r):bs)
zipLeft (Zipper (_, bs)) = Snag

zipRight :: AEZipper a -> AEZipper a
zipRight Snag = Snag
zipRight (Zipper (Expr o l r, bs)) = Zipper (r, (RExpr o l):bs)
zipRight (Zipper (_, bs)) = Snag

zipDown :: AEZipper a -> AEZipper a
zipDown Snag = Snag
zipDown (Zipper (Para e, bs)) = Zipper (e, DPara:bs)
zipDown (Zipper (_, bs)) = Snag

zipUp :: AEZipper a -> AEZipper a
zipUp Snag = Snag
zipUp (Zipper (e, [])) = Snag
zipUp (Zipper (e, (LExpr o r):bs)) = Zipper (Expr o e r, bs)
zipUp (Zipper (e, (RExpr o l):bs)) = Zipper (Expr o l e, bs)
zipUp (Zipper (e, DPara:bs)) = Zipper (Para e, bs)

zipTop :: AEZipper a -> AEZipper a
zipTop Snag = Snag
zipTop (Zipper (e, [])) = Zipper (e, [])
zipTop (Zipper (e, cs)) = zipTop $ zipUp (Zipper (e, cs))

-- Manipulate the part of the tree under focus

zInsert :: AExpr a -> AEZipper a -> AEZipper a
zInsert _ Snag = Snag
zInsert e (Zipper (_, cs)) = Zipper (e, cs)
