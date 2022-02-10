Defining functions

4.1
 
> even :: Integral a => a -> Bool
> even n = n `mod` 2 == 0


> splitAt :: Int -> [a] -> ([a], [a])
> splitAt n xs = (take n xs, drop n xs)

> recip :: Fractional a => a -> a
> recip n = 1 / n

4.2 Conditional Expression

> abs :: Int -> Int
> abs n = if n >= 0 then n else -n


> signum :: Int -> Int
> signum n = if n < 0
>               then -1
>            else
>               if n == 0
>                  then 0
>               else 1

!! Conditional expression in Haskell must always have en `else` branch,
which avoids the well-known dangling else problem.

4.3 Guarded equations

> signum' n
>    | n < 0     = -1
>    | n == 0    = 0
>    | otherwise = 1

4.4 Pattern matching

> not :: Bool -> Bool 
> not False = True
> not True = False


Tuple patterns

> fst :: (a, b) -> a
> fst (x, _) = x


> snd :: (a, b) -> b
> snd (_, y) = y

List patterns

> test :: [Char] -> Bool
> test ['a', _, _]  = True
> test _            = False

[1, 2, 3]
= 1 : [2, 3]
= 1 : (2 : [3])
= 1 : (2 : 3: [])

> head :: [a] -> a
> head [] = error "Empty list"
> head (x:_) = x

> tail :: [a] -> [a]
> tail [] = []
> tail (_:xs) = xs

4.5 Lambda expressions

> add :: Int -> Int -> Int
> add = \x -> \y -> x + y

> const :: a -> b -> a
> const x _ = x

> const' :: a -> b -> a
> const' x = \_ -> x

> odds :: Int -> [Int]
> odds n = map f [0..n-1]
>           where f x = x * 2 + 1

4.6 Operator sections

if # is an operator, then expressions of the form (#), (x #), and (# y) for arguments
x and y are claaed sctions: 

(#) = \x -> (\y -> x # y)

(x #) = \y -> x # y

(# y) = \x -> x # y

