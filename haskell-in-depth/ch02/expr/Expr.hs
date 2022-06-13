module Expr where

import TextShow

data Expr a where
    Lit :: a -> Expr a
    Add :: Expr a -> Expr a -> Expr a
    Mult :: Expr a -> Expr a -> Expr a
    deriving (Show, Read)

eval :: Num a => Expr a -> a
eval (Lit e) = e
eval (Add e1 e2) = eval e1 + eval e2
eval (Mult e1 e2) = eval e1 * eval e2

expr1, expr2 :: Expr Int
expr1 = Mult (Add (Lit 2) (Mult (Lit 3) (Lit 3))) (Lit 5)
expr2 = Add (Add (Lit 1)
                 (Mult (Add (Lit 1) (Lit 2))
                       (Add (Lit 2)
                            (Mult (Lit 2) (Add (Lit 1) (Lit 2))))))
            (Add (Lit 1) (Mult (Lit 3) (Lit 2)))

instance TextShow a => TextShow (Expr a) where
    showbPrec p e =
        case e of
            Lit a -> showb a
            Add e1 e2 -> showUtil p 5 "+" e1 e2
            Mult e1 e2 -> showUtil p 6 "*" e1 e2
        where
            showUtil outerPrec curPrec op e1 e2 =
                showbParen (outerPrec > curPrec)
                    $ showbPrec curPrec e1 <> op <> showbPrec curPrec e2
