import AExpr
import AEZipper

expr :: (Fractional a) => AExpr a
expr = Expr Add
        (Expr Add
            (Expr Mul
                (Val 10)
                (Val 2))
            (Val 99))
        (Expr Mul
            (Expr Div
                (Val 10)
                (Val 9))
            (Para
                (Expr Sub
                    (Para
                        (Expr Add
                            (Val 20)
                            (Val 9)))
                    (Val 9))))

z :: (Fractional a) => AEZipper a
z = Zipper (expr, [])

sequence0 :: (Fractional a) => AEZipper a
sequence0 = z -: (zipRight .> zipLeft .> zipRight .> zInsert (Val 100) .> zipTop)

sequenceLi :: (Fractional a) => [AEZipper a -> AEZipper a]
sequenceLi = [zipRight, zipLeft, zipRight, zInsert (Val 100.0), zipTop]
