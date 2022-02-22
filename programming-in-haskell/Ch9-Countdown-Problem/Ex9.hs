module Ex9 where

import Lib ( choices, perms, subs, exprs, eval, Expr )

-- 1.

choices' :: [a] -> [[a]]
choices' xs = [ps | ss <- subs xs, ps <- perms ss]


-- 2.

isChoice :: Eq a => [a] -> [a] -> [Bool]
isChoice [] _ = [True]
isChoice (x:xs) ys
    | x `elem` ys = isChoice xs ys
    | otherwise = [False]


isChoice' xs ys = (:[]) . or $ [xs == ps | ss <- subs ys, ps <- perms ss]


-- 3.

nums :: [Int]
nums = [1, 3, 7, 10, 25, 50]
 
possibleExprs ns = length [ e | ns' <- choices ns, e <- exprs ns']

correctExprs ns = length [i | ns' <- choices ns, e <- exprs ns', i <- eval e]