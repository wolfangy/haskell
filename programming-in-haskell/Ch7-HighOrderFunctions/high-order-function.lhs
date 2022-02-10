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
