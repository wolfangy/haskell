module Ex where

-- 1. 
fac n 
    | n == 0 = 1
    | n > 0  = n * fac (n - 1)
    | otherwise = error "undefined"

-- 2. 
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 3.
exp' :: Int -> Int -> Int
n `exp'` 1 = n
n `exp'` m = n * (n `exp'` (m-1))