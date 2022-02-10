Recursie functions

fac :: Int -> Int
fac 0 = 1               -- the base case
fac n = n * fac (n-1)   -- the recursive case


6.2 Recursion on list

> product' :: Num a => [a] -> a
> product' [] = 1
> product' (n:ns) = n * product' ns

> reverse' :: [a] -> [a]
> reverse' [] = []
> reverse' (x:xs) = reverse' xs ++ [x]


> (<->) :: [a] -> [a] -> [a]
> [] <-> ys = ys
> xs <-> [] = xs
> (x: xs) <-> ys = x : (xs <-> ys)

> insert :: Ord a => a -> [a] -> [a]
> insert n [] = [n]
> insert n (x:xs)
>           | n <= x     = n : x : xs
>           | otherwise  = x : insert n xs 

> isort :: Ord a => [a] -> [a]
> isort [] = []
> isort (x:xs) = insert x $ isort xs

> isort' :: Ord a => [a] -> [a]
> isort' = foldr insert []

> init' :: [a] -> [a]
> init' [] = error "Cannot process empty array"
> init' [_] = []
> init' (x:xs) = x : init' xs

6.3 Multiple arguments 

> zip' :: [a] -> [b] -> [(a, b)]
> zip' _ [] = []
> zip' [] _ = []
> zip' (x:xs) (y:ys) = (x, y) : zip' xs ys


6.4 Multiple Recursion

> fib :: Int -> Int
> fib 0 = 0
> fib 1 = 1
> fib n = fib (n-2) + fib (n-1)

> qsort :: Ord a => [a] -> [a]
> qsort [] = []
> qsort (x:xs) = qsort (filter (<=x) xs) ++ [x] ++ qsort (filter (>x) xs)

6.5 Mutual recursion

> even' :: Int -> Bool 
> even' n
>      | n == 0 = True
>      | otherwise = odd' (n-1)

> odd' :: Int -> Bool
> odd' n
>       | n == 0 = False
>       | otherwise = even' (n -1)