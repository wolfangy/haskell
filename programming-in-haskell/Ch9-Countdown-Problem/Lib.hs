{-# LANGUAGE GADTs #-}

module Lib where
import Distribution.Types.LocalBuildInfo (componentNameTargets')

data Op where
  Add :: Op
  Sub :: Op
  Mul :: Op
  Div :: Op

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"


-- decides if the application of an operator
-- to two positive naturals gives another positive
-- natural
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

-- perform an application of an operator
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr where
  Val :: Int -> Expr
  App :: Op -> Expr -> Expr -> Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = bark l ++ show o ++ bark r
    where
      bark (Val n) = show n
      bark e = "(" ++ show e ++ ")"

-- returns the list of values in an expression
values :: Expr -> [Int]
values (Val v) = [v]
values (App o l r) = values l ++ values r

-- returns the overall value of an expression
-- provided that this value is a positive natural number.
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y |
                        x <- eval l,
                        y <- eval r,
                        valid o x y]

-- returns all subsequences of a list
-- which are given by all possible combinations
-- of excluding or including each element fo the list.
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
    where yss = subs xs

-- returns all possible ways of inserting a new element into a list
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y :) (interleave x ys)

-- return all permutations of a list, which
-- are given by all possible reordering of the elements
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- returns all permutations of all subsequences
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- to define a function that formalises what it means to solve an instance
-- of the countdown problem.
solution :: Expr -> [Int] -> Int -> Bool 
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- returns all possible ways of splitting a list into two
-- non-empty lists that append to give the original list
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) =([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                    l <- exprs ls,
                    r <- exprs rs,
                    e <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = 
    [e |  ns' <- choices ns,
            e <- exprs ns',
            eval e == [n]]


type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                     lx      <- results ls,
                     ry      <- results rs,
                     res     <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (ex, vx) (ey, vy) =
    [(App o ex ey, apply o vx vy) | o <- ops, valid' o vx vy]

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0


solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
    [e | ns' <- choices ns, (e, m) <- results ns', m == n]