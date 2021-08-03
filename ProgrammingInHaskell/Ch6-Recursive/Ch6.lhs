> module Ch6 where

> reverse' :: [a] -> [a]
> reverse' [] = []
> reverse' (x:xs) = reverse' xs ++ [x]


> (+<) :: [a] -> [a] -> [a]
> [] +< ys = ys
> xs +< [] = xs
> (x: xs) +< ys = x : (xs +< ys)

> insert :: Ord a => a -> [a] -> [a]
> insert n [] = [n]
> insert n (x:xs)
>           | n <= x     = x : n : xs
>           | otherwise  = x : insert n xs 

> isort :: Ord a => [a] -> [a]
> isort [] = []
> isort (x:xs) = insert x $ isort xs

> isort' :: Ord a => [a] -> [a]
> isort' = foldr insert []

* Multiple arguments 

> zip' :: [a] -> [b] -> [(a, b)]
> zip' _ [] = []
> zip' [] _ = []
> zip' (x:xs) (y:ys) = (x, y) : zip' xs ys


* Multiple Recursion

> fib :: Int -> Int
> fib 0 = 0
> fib 1 = 1
> fib n = fib (n-2) + fib (n-1)

> qsort :: Ord a => [a] -> [a]
> qsort [] = []
> qsort (x:xs) = qsort (filter (<=x) xs) ++ [x] ++ qsort (filter (>x) xs)

* Mutual recursion

> even' :: Int -> Bool 
> even' n
>      | n == 0 = True
>      | otherwise = odd' (n-1)

> odd' :: Int -> Bool
> odd' n
>       | n == 0 = False
>       | otherwise = even' (n -1)