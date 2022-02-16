{-# LANGUAGE GADTs #-}

module Expr where

data Expr where
    Val :: Int -> Expr
    Add :: Expr -> Expr -> Expr
    deriving(Eq, Show)

value :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y

{-
value (Add (Add (Val 2) (Val 3)) (Val 4))
= value (Add (Val 2) (Val 3)) + value (Val 4)
= (value (Val 2) + value (Val 3)) + value (Val 4)
= (2 + value (Val 3)) + value (Val 4)
= (2 + 3) + value (Val 4)
= 5 + value (Val 4)
= 5 + 4
= 9

The order of evaluation is determined by Haskell.

If desired, however, such control information can
be made explicit by defining a abstract machine for
expression, which specifies the step-by-step process
of their evaluation.

-}

data Op where
    EVAL :: Expr -> Op
    ADD :: Int -> Op

type Cont = [Op]

-- evaluates an expression in the context of a control stack:

eval :: Expr -> Cont -> Int

eval (Val n) c = exec c n
-- if the expression is integer:
-- it is already fully evaluated
-- we begin executing the control stack.

eval (Add x y) c = eval x (EVAL y : c)
-- if the expression is addition:
-- we evaluate the first argument x
-- then placing the operation EVAL y
-- on top of the control stack to indicate
-- that the second argument y should be evaluated
-- once the evaluation of the first argument is completed.

exec :: Cont -> Int -> Int
exec [] n           = n
-- if the control stack is empty,
-- we return the integer argument as the result.

exec (EVAL y : c) n = eval y (ADD n : c)
-- if the top of the stack is an EVAL y,
-- then we evaluate the expression y,
-- placing the operation ADD n on top of 
-- the remaining stack to indicate that the 
-- current integer argument n should be added
-- together with the result of evaluating y
-- once this is completed.
exec (ADD n : c) m  = exec c (n + m)
-- if the top of the stack is an operation ADD n,
-- evaluation of the two arguments of an addition
-- expression is now complete, and we execute the
-- remaining control stack in the context of the sum
-- of the two resulting integer values.

process :: Expr -> Int
process e = eval e []

c1 :: Expr
c1 = Add
        (Add
            (Val 2)
            (Val 3))
        (Val 4)

-- process c1
{-
= eval (Add (Add (Val 2) (Val 3)) (Val 4)) []
= eval (Add (Val 2) (Val 3)) [EVAL (Val 4)]
= eval (Val 2) [EVAL (Val 3), EVAL (Val 4)]
= exec [EVAL (Val 3), EVAL (Val 4)] 2
= eval (Val 3) [ADD 2, EVAL (Val 4)]
= exec [ADD 2, EVAL (Val 4)] 3
= exec [EVAL (Val 4)] 5
= exec [ADD 5] 4
= exec [] 9
= 9
-}