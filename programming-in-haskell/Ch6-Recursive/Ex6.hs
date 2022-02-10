module Ex6 where

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

-- 4.

euclid :: Int -> Int -> Int
euclid x y
        | x == y = x
        | x < y = euclid x (y - x)
        | x > y = euclid (x - y) y
        | otherwise = error "Invalid"


-- 6.

and' :: [Bool] -> Bool
and' = foldr (&&) True

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ (concat' xss)

replicate' :: Int -> a -> [a]
replicate' 0 x = [x]
replicate' n x = x : replicate' (n - 1) x


(#) :: [a] -> Int -> Maybe a
(#) [] _ = Nothing
(#) (x:_) 0 = Just x
(#) (x:xs) n = xs # (n - 1)


elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs)
    | n == x    = True
    | otherwise = elem' n xs

-- 7. 

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xss@(x:xs) yss@(y:ys)
    | x <= y     = x : merge xs yss
    | otherwise  = y : merge xss ys


-- 8. 

msort :: Ord a => [a] -> [a]
msort []    = []
msort [x]   = [x]
msort xs    = merge (msort left) (msort right)
    where
        half    = length xs `div` 2
        left    = take half xs
        right   = drop half xs

-- 9.

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

last' :: [a] -> Maybe a
last' [] = Nothing 
last' [x] = Just x
last' (x:xs) = last' xs