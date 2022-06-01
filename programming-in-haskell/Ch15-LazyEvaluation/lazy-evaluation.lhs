> {-# LANGUAGE GADTs #-}

15.1 Introduction

The basic method of computation in Haskell is the application of functions to 
arguments.

    inc :: Int -> Int
    inc n = n + 1

the expression `inc (2 + 3)` can be evaluated:

    inc (2 * 3)
=   {applying *}
    inc 6
=   {applying inc}
    6 + 1
=   {applying +}
    7

Alternatively, the function application could in opposite order:
    inc (2 * 3)
=   {applying inc}
    (2 * 3) + 1
=   {applying *}
    6 + 1
=   {applying +}
    7

!!! The fact that changing the order in which functions are applied does not affect 
the final result.

!!! This property will not hold for most imperative programming languages.

15.2 Evaluation strategies

An expression that has the form of a function applied to one or more arguments that 
can be 'reduced' by performing the application is called a `reducible expression`, 
or `redex` for short.

!!! reduction do not necessarily decrease the size of an expression.

    mult :: (Int, Int) -> Int
    mult (x, y) = x * y

The expression `mult (1+2, 2+3)` contains three redexes: `1 + 2`, `2 + 3` and 
`mult (1+2, 2+3)`

Performing the corresponding reductions gives the expressions:
    mult (3, 2+3)
    mult (1+2, 5)
    (1+2) * (2+3)

When evaluating an expression?

* `innermost evaluation`: always choose a redex that is innermost, if there is more 
that one innermost, by convention we choose the one that begins at the leftmost 
position.

!!! Innermost evaluation can also be characterised in terms of how arguments are 
passed to functions: it ensures that arguments are always fully evaluated before 
functions are applied - arguments are passed by `value`.

* `outermost evaluation`: from outside towards inside, from left to right.

!!! Outermost evaluation allows functions to be applied before their arguments are 
evaluated - arguments are passed by `name`.

 * Many functions require their arguments to be evaluated before being applied, 
 even when using outermost evaluation. Function with this property are called `strict`.


 Lambda expressions

    mult :: Int -> Int -> Int
    mult x = \y -> x * y

using the innermost evaluation:

    mult (1+2) (2+3)
=   {applying the first +}
    mult 3 (2+3)
=   {applying mult}
    (\y -> 3 * y) (2+3)
=   {applying +}
    (\y -> 3 * y) 5
=   {applying the lambda}
    3 * 5
=   {applying *}
    15

Note in Haskell, the selection of redexes within the bodies lambda expression is 
prohibited. The only operation that can be performed on a function is that of applying 
it to an argument. As such, reduction within the body of a function is only permitted 
once the function has been applied.

    \x -> 1 + 2
is deemed to already be fully evaluated, even though the body contains the redex `1 + 2`

Using the `innermost` and `outermost` evaluation, but NOT within lambda expressions,
is normally referred to as `call-by-value` and `call-by-name` evaluation.

15.3 Termination

    inf :: Int
    inf = 1 + inf

the integer `inf` is defined as teh successor of itself.

    fst (0, inf)

Using `call-by-value` evaluation, will result non-termination in a similar manner
    fst (0, inf)
=   {applying inf}
    fst (0, 1 + inf)
=   {applying inf}
    fst (0, 1 + (1 + inf))
    ...

Using `call-by-name` evaluation results termination with the result `0`
    fst (0, inf)
=   {applying fst}
    0

!!! If there exists any evaluation sequence that terminated for a given expression, 
then call-by-name evaluation will also terminate for this expression, and produce 
the same final result.

`call-by-name` evaluation is preferable to `call-by-value` for the purpose of ensuring
that evaluation terminates as often as possible.

15.4 Number of reductions

In contrast, using `call-by-name` evaluation may require more reduction steps than
`call-by-value` evalution, in particular when an arugment is used more than once 
in the body of a function.

!!! Arguments are evaluated precisely once using `call-by-value` evaluation, but 
may be evaluated many times using call-by-name.

The efficiency problem with `call-by-name` evaluation can be easily solved by using 
pointers to indicate sharing of expressions during evaluation, rather than physically 
copying an argument if it is used many times in the body of a function.

    square (1 + 2)
=   {applying square}
    . * .  [1 + 2]
=   {applying +}
    . * .  3
=   {applying *}
    9

keep only a single copy of `1+2` and make two pointers to it.

!!! The use of `call-by-name` evaluation in conjunction with sharing is known as 
`lazy evaluation`.

Lazy evaluation being based upon:
    * using `call-by-name` evaluation ensures that evaluation terminates as often 
    as possible.

    * using `sharing` ensures that lazy evalutions never requires more steps than
    `call-by-value` evaluation.

15.5 Infinite structures

An additional property of lazy evaluation (by `call-by-name`):
programming with infinite structures.

> ones :: [Int]
> ones = 1 : ones

the list `ones` is defined as a single one followed by itself.

    ones
=   {applying ones}
    1 : ones
=   {applying ones}
    1 : (1 : ones)
=   {applying ones}
    1 : (1 : (1 : ones))
    ...

evaluating `ones` will produce a never-ending list of ones.

`head` function defined as:
    head (x:_) = x

using lazy evaluation results in termination in two steps:

    head ones
=   {applying ones}
    head (1 : ones)
=   {applying head}
    1

Lazy evaluation proceeds in a lazy manner, only evaluating arguments as and when 
this is strictly necessary in order to produce results.

`head` only select the first element of a list, the remainder of the list is not 
required, and hence in `head (1 : ones)` the further evalution of the infinite 
list is not required.

!!! Under lazy evaluation `ones` is not an infinite list as such, but rather a 
`potentially infinite` list, which is only evaluated as much as required by the 
context.

15.6 Modular programming

Lazy evaluation allows us to separate `control` from `data` in our computations.

Like: take first three elements (control) of the infinite list of `ones` (data)

    take 3 ones
    [1,1,1]

    take 3 ones
=   {applying ones}
    take 3 (1 : ones)
=   {applying take}
    1 : take 2 ones
=   {applying ones}
    1 : take 2 (1 : ones)
=   {applying take}
    1 : 1 : take 1 ones
=   {applying ones}
    1 : 1 : take 1 (1 : ones)
=   {applying take}
    1 : 1 : 1 : take 0 ones
=   {applying take}
    1 : 1 : 1 : []
=   {list notation}
    [1, 1, 1]

the data is only evaluated as much as required by the control, and two parts take 
it in turn to perform reduction.

Being able to modularise programs by separating then into logically distinct parts 
is an important goal in programming, and being able to separate control from data 
is one of the most important benefits of Lazy Evaluation.

!!! Care is still required when programming with infinite list

Non-termination:
    filter (<= 5) [1..]
the function `filter` keeps testing elements of the infinite list in vain attempt 
to find another that is less than or eqal to five.

Termination:
    takeWhile (<= 5) [1..]
`takeWhile` stops as soon as it finds an element that is greater than five.

`Sieve of Eratosthenses`: Generating the infinite sequence of all prime numbers:

1. write down the inifite sequence: 2, 3, 4, ...;
2. mark the first number, `p`, in the sequence as prime;
3. delete all multiples of `p` from the sequence;
4. return to the `step 2`

> sieve :: [Int] -> [Int]
> sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

> primes :: [Int]
> primes = sieve [2..]

15.3 Strick application

Haskell provides a special strick version of funtion application: "$!"

an epxression: `f $! x` behaves in the same way as normal function application 
`f x`, except that the top-level of evaluation of the argument expression `x` is
forced before the function `f` is applied.

