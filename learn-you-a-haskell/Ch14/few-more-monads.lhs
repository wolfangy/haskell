> module FewMoreMonads where

With Cabal:

To `cabal repl` with external module:

1. install the module with command:

    cabal install --lib mtl

2. lauch the `cabal repl` with the module by:

    cabal repl --ghc-option='-package mtl'

With Stack:

    stack ghci --pakcage mtl

> import Control.Monad.Writer
> import Prelude hiding (gcd)


1. Writer

The `Writer` monad is for values taht have another value attached that acts as a 
sort of log value.

`Writer` allows us to do the computations while making sure that all the log values 
are combined into one log value, which then is attached to the result.

> isBigGang :: Int -> Bool
> isBigGang x = x > 9

> isBigGang' :: Int -> (Bool, String)
> isBigGang' x = (x > 9, "Compared gang size to 9.")

with the return `tuple`, some context has been added to our value now.

make a function that takes a value with an attached log, and it will guarantee that 
the log of the original value isn't lost, but is joined together with the log of 
the value that results from the function.

> applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
> applyLog (x, log) f = let (y, newLog) = f x
>                       in (y, log ++ newLog)

> applyLog' :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
> applyLog' (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

The `Writer` type

The definition is:

    newtype Writer w a = Writer { runWriter :: (a, w) }

* The `Writer` constructor has NOT been exported, however it does export the 
`writer` function, which does the same thing as the Writer constructor would do.

* You cannot pattern match against `Writer` constructor, instead, you need to use 
the `runWriter` function, which takes a tuple that's wrapped in a `Writer newtype` 
and unwraps it, returning a simple tuple.

Use `do` notation with Writer

> logNumber :: Int -> Writer [String] Int
> logNumber x = writer (x, ["Got number: " ++ show x])

> multWithLog :: Int -> Int -> Writer [String] Int
> multWithLog x y = do
>   a <- logNumber x
>   b <- logNumber y
>   tell ["Gonna multiply " ++ show a ++ " with " ++ show b ++ "."]
>   return (a*b)

Adding Loggin to Programs


> gcd :: Int -> Int -> Writer [String] Int
> gcd a b
>   | b == 0 = do
>       tell ["Finished with " ++ show a]
>       return a
>   | otherwise = do
>       tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
>       gcd b (a `mod` b)

> printLog :: Writer [String] a -> IO ()
> printLog = (mapM_ putStrLn) . snd . runWriter