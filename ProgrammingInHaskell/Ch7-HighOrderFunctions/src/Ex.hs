module Ex where

-- 1
mapFilter = map . filter

-- 2

-- a
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr ((&&) . p) True


-- 3
mapf f = foldr ((:) . f) []

filterf p = foldr (\a b -> if p a then a:b else b) []

-- 4

dec2int :: [Int] -> Integer 
dec2int ds = sum [fromIntegral (w * b) | (w, b) <- zip weights ds]
    where weights = iterate (*10) 1

-- 5

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b 

