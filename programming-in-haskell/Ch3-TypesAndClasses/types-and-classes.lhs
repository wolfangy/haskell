Types and classes

3.1

Every expression must have a type, which is calculated prior to evaluating the expression
by a process called type inferecence.

Because type inference precedes evaluation, Haskell programs are type safe, in the sense that type
errors can never occur during evaluation.

3.2 Basic types

* Bool 

* Char

* String

* Int

* Integer 

* Float 

* Double

3.3 List types


3.4 Tuple types

A tuple is a finite sequence of components of possibly different types


3.5 Function types

!! Not that there is no restrction that functions must be total on their argument type, in the
sense that there may be some arguments for which the result is not defined.

3.6 Curried functions

Curried functions are also more flexible than functions on typles, because useful functions can 
often be made by partially applying acurried function with less than its full complement of arguments.

!! the function arrow -> in type is assumed to assiciate to the right

3.6 Polymorphic types

A type that contains one or more type variables is called polymorphic ("of many forms"),
as is an an expresion with such a type

3.7 Overloaded types

Class constraints are written in the form C a, where C is the name of a class and a is a type variable.

e.g: (+) :: Num a => a -> a -> a

3.9 Basic classes

a class is a collection of types that support certain overloaded operations called methods.

* Eq: 
(==)
(/=)

* Ord:
(<)
(<=)
(>)
(>=)
min :: a -> a -> a
max :: a -> a -> a

* Show:
show :: a -> String

* Read:
read :: String -> a

* Num:
(+)
(-)
(*)
negate
abs
signum: return the sign

* Integral - integral types:
This class contains types that are instances of the numberic class Num, but in addition
whose values are integers, and as such support the methods of integer division and integer
remainder.

div :: a -> a -> a

mod :: a -> a -> a

* Fractional
This class contans types that are instances of the numeric class Num, but in 
addition whose values are non-integral, and as such support the methods of 
fractional division and fractional reciprocation.

(/)

recip

3.11 Exercise

> e1_1 :: [Char]
> e1_1 = ['a', 'b', 'c']

> e1_2 :: (Char, Char, Char)
> e1_2 = ('a', 'b', 'c')

> e1_3 :: [(Bool , Char)]
> e1_3 = [(False , '0'), (True, '1')]

> e1_4 :: ([Bool], [Char])
> e1_4 = ([False, True], ['0', '1'])

> e1_5 :: [[a]-> [a]]
> e1_5 = [tail, init, reverse]


2.

> bools :: [Bool]
> bools = [True, False]

> nums :: [[Int]]
> nums = [[1], [1,2], [1..3]]

> add :: Int -> Int -> Int -> Int
> add a b c = a + b + c

> copy :: a -> (a,a)
> copy v = (v, v)

> apply :: (a -> b) -> a -> b
> apply f = f 


3.

> second :: [a] -> a
> second = head . tail

> swap :: (a, b) -> (b, a)
> swap (a, b) = (b, a)

> pair :: a -> b -> (a, b)
> pair x y = (x, y)

> double :: Num a => a -> a
> double = (*2)

> palindrom :: Eq a => [a] -> Bool
> palindrom xs = reverse xs == xs

> twice :: (a -> a) -> a -> a
> twice f = f . f