module Expr where

data Expr = Val Int | Add Expr Expr deriving(Eq, Show)

value :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y

data Op = EVAL Expr | ADD Int

type Cont = [Op]

exec :: Cont -> Int -> Int
exec [] n           = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m  = exec c (n + m)

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)


c1 = Add
        (Add
            (Val 2)
            (Val 3))
        (Val 4)