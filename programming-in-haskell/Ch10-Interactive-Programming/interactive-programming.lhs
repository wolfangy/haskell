> module Text where

10.2 The solution

an interactive program is viewed as a pure function that takes the current 
state of the world as its argument, and produces a modified world as its 
result, in which the modified world reflects any side effects that were 
performed by the program during its execution.

    type IO = World -> World

An interactive program may return a result value in addition to perform side effects.

    type IO a = World -> (a, World)

Expression of type IO a are called actions.


An interactive program may also require argument values. However, there is no need 
to generalise the IO type further to take account of this.

take a character and return an integer would have type: 

    Char -> World -> (Int, World)


10.3 Basic actions

* Read a char from keyboard:

    getChar :: IO Char

* Write the char to screen:

    putChar :: Char -> IO ()

* Return the value in action:

    return :: a -> IO a

!! the return provides a bridge from pure expressions without side effects to impure 
actions with side effects.


10.4 Sequencing

A sequence of IO actions can be combined into a single composite action using 
the `do` notation

    do
        v1 <- a1 
        v2 <- a2
        ...
        vn <- an
        return (f v1 v2 ... vn)

1. the layout rule applies, each action in the sequnece must begin in the same column

2. the expression `vi <- ai` is called generator, as the list comprehensions

3. if the value produced by generator is not required, can be abbreviated as 
    `_ <- ai`

action reads three characters, discard the second one:

> readTwo:: IO (Char, Char)
> readTwo = do
>              x <- getChar
>              getChar
>              y <- getChar
>              return (x, y)

10.5 Derived Primitives

> getLine' :: IO String
> getLine' = do
>           x <- getChar
>           if x == '\n' then return []
>           else
>               do
>                   xs <- getLine'
>                   return (x:xs)

> putStr' :: String -> IO ()
> putStr' [] = return ()
> putStr' (x:xs) = do
>                   putChar x
>                   putStr' xs

> putStrLn' :: String -> IO ()
> putStrLn' xs = putStr' xs >> putChar '\n'

> strlen :: IO ()
> strlen = do
>           putStr "Enter a string:"
>           xs <- getLine'
>           putStr' "The string has "
>           putStr' . show .length $ xs
>           putStrLn' " characters."


