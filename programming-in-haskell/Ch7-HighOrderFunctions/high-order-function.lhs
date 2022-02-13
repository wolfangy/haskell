Higher-order functions


7.1 Basic

A function taht takes a function as an argument or returns a function as result
 is called a higher-order function.

!! Higher-order functions can be used to define domain-specific languages within Haskell

7.2 Processing lists

map :: (a -> b) -> f a -> f b

f a could be a [a]

map f []        = []
map f (x:xs)    = f x : map f xs

> add1 = map (+1)

process nested list:

> add1Nested = map (map (+ 1)) [[1..3], [4..6]]

> allEven = map even

> reverseAll = map reverse


filter :: (a -> Bool) -> [] a -> [] a
filter p [] = []
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs


* Decide if all elements of a list satisfy a predicate:

> allEven2 = all even [2, 4, 6, 8]

* Decide if any

> anyOdd = any odd [2, 4, 6, 8]


* Select element from a list while they satisfy a predicate:

> takeAll = takeWhile even [2, 4, 6, 8]


* Remove elements from a list while they 

> dropAll = dropWhile even [2, 4, 6, 8]


7.3 foldr

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v []        = v
foldr f v (x:xs)    = f x (foldr f v xs)

It is best to think of the behavior of foldr f v in a non-recursive manner, as simply replacing
each cons operator in a list by the function f, and the empty list at the end by the value v.

[1, 2, 3]               ==> 1 : (2 : (3: []))

foldr (+) 0 [1, 2, 3]   ==> 1 + (2 + (3 + 0))

!! fold right reflects the use of an operator that is assumed to associated to the right:
foldr (#) v [x0, x1, ... ,xn] = x0 # (x1 # (... (xn # v) ...))

7.4 foldl

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f v []     = v
foldl f v (x:xs) = foldl f (f v x) xs


foldl (#) v [x0, x1, x2, ..., xn] = (... ((v # x0) # x1) ...) # xn


7.5 The composition operator

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

composition is associative:
f . (g . h) = (f . g) . h

Composition also has an identity:

id :: a -> a
id = \x -> x


The identity function is often useful when reasoning about program, and also provides a 
suitable starting point for a sequence of compositions

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id


7.6 Binary  string transmitter
