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
> import Control.Monad.State
> import Control.Monad.Except
> import Prelude hiding (gcd)


1. Writer

The `Writer` monad is for values that have another value attached that acts as a 
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


2. Reader

The function type `(->) r` is an instance of Functor:
Mapping a function `f` over a function `g` will make a function that takes the same 
thing as `g`, applies `g` to it, and then applies `f` to that result.
    
    fmap f g == f . g

The function type `(->) r` is an instance of Applicative:
They allow us to operate on the eventual results of functions as if we already had 
their results.

> applicative_f = (+) <$> (* 2) <*> (+ 10)

The function type as Monads

    instance Monad ((->) r) where
        return x = \ _ -> x
        f >>= g = \x -> g (f x) x

> addStuff :: Int -> Int
> addStuff = do
>   a <- (*2)
>   b <- (+10)
>   return (a + b)

The function monad is also called the `reader` monad.

All the functions read from a common source, reader monad allows us to tread functions 
as values with a context.

We can act as if we already know the function will return, it does this by gluing 
functions together into one function and then giving that function's parameter to 
all of the functions that compose it.

So if we have a lot of functions that are all just missing one parameter, we will 
eventually be applied the the same thing, we can use the reader monad to sort of 
extract their future results, and the `>>=` implementation will make sure that it 
all works out.

3. State

Stateful computation is a function that takes some state and returns a value along 
with some new state.

    s -> (a, s)

`s` is the type of the state
`a` is the result of the stateful computation

> type Stack = [Int]

> pop :: Stack -> (Int, Stack)
> pop (x:xs) = (x, xs)

> push :: Int -> Stack -> ((), Stack)
> push a xs = ((), a : xs)

> stackManip :: Stack -> (Int, Stack)
> stackManip stack = let
>   ((), newStack1) = push 3 stack
>   (a, newStack2) = pop newStack1
>   in pop newStack2

The definition of `State` Monad:

    newtype State s a = State { runState :: s -> (a, s) }

A `State s a` is a stateful computation that manipulates a type `s` ans has a result 
of type `a`.

!!! `State s a` does not export its value constructor, if you need to take a stateful 
computation and wrap it in the `State`, use the `state` function.

State is a monad:

    instance Monad (State s) where
        return :: a -> State s a
        return x = State $ \s -> (x, s)

        (>>=) :: State s a -> (a -> State s b) -> State s b
        (State h) >>= f = State $ \s -> let (a, newState) = h s
                                            (State g)     = f a
                                        in
                                            g newState

> popM :: State Stack Int
> popM = state $ \(x:xs) -> (x, xs)

> pushM :: Int -> State Stack ()
> pushM a = state $ \(xs) -> ((), a:xs)

> stackManipM :: State Stack Int
> stackManipM = do
>   pushM 3
>   popM
>   popM

> push_3_8_unless_5 :: State Stack ()
> push_3_8_unless_5 = do
>   a <- popM
>   if a == 5
>       then pushM 5
>       else do
>           pushM 3
>           pushM 8

Getting and Setting State

The `Control.Monad.State` module provides a type class called `MonadState`, which 
features two pretty useful functions:

`get` takes the current state and present it as the result:

    get = state \s -> (s, s)

`put` takes some state and makes a stateful function that replaces the current state 
with it:

    put newState = state $ \s -> ((), newState)

> stackyStack :: State Stack ()
> stackyStack = do
>   stackNow <- get
>   if stackNow == [1, 2, 3]
>       then put [8, 3, 1]
>       else put [9, 2, 1]

% > popM' :: State Stack Int
% > popM' = do
% >   xs <- get
% >   put $ drop 1 xs
% >   case xs of
% >       []    -> 0
% >       (x:_) -> x

> pushM' :: Int -> State Stack ()
> pushM' a = do
>   xs <- get
>   put (a:xs)

4. Either

`Either` is also an instance of the MonadError type class, in `Control.Monad.Error`

This type class is for monads whose values can fail and provide some sort of data 
with their failure.

1. `throwError` takes some sort of error data and returns a valud that fails with that 
data.

> either_error = throwError "warp core breach imminent!" :: Either String Int

2. `catchError` takes two parameter:

    * The first one is a monadic value that can fail.

    * The second one is a function that is evaluated if the given monadic value 
    has failed

> catch_error = Left "Oops!" `catchError` (\exp -> throwError $ "Aborting with Error: " ++ exp)
> no_catch = Right 100 `catchError` (\exp -> throwError $ "Aborting with Error: " ++ exp)

5. Some Useful Monadic functions

5.1 liftM and Friends

`liftM` takes a function and a monadic value and maps the function over the monadic 
value:

    liftM :: (Monad m) => (a -> b) -> m a -> m b

> just_24_M = liftM (*3) (Just 8)
> just_24_F = fmap  (*3) (Just 8)

> false_writer_M = runWriter $ liftM not $ writer (True, "chickpeas")
> false_writer_F = runWriter $ fmap  not $ writer (True, "chickpeas")

> state_101_M = runState (liftM (+100) popM) [1..4]
> state_101_F = runState (fmap  (+100) popM) [1..4]

`ap` function is basically <*>, but with a Monad constraint instead of an Applicative 
one.

    ap :: (Monad m) => m (a -> b) -> m a -> m b

    (<*>) :: (Applicative f) -> f a -> f b

`liftA2` is a convinient function for applying a function between two applicative 
values.

    liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
    liftA2 f x y = f <$> x <*> y

`liftM2` does the same thing but with a Monad

5.2 The `join` Function

    join :: (Monad m) => m (m a) -> m A

`join` takes a monadic value within a monadic value and give us just one monadic 
value - it flattens it.

> flatten_list = join [[1..4], [5..9]]

> flatten_writer = runWriter $ join (writer (writer (1, "aaa"), "bbb"))

!!! `join` is that for every monad, feeding a monadic value to a function with `>>=` 
is the same as just mapping that function over the value and then using the `join` 
to flatten the resulting nested monadic value.

    m >>= f :== join (fmap f m)

!!! `join` cannot be implemented by just using the function that functors and applicatives 
provide.

5.3 `mapM`, `mapM_`, `forM` and `forM_`

    mapM  :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)

    mapM_ :: (Foldable t, Monad m)    => (a -> m b) -> t a -> m ()


    forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)

    forM_ :: (Foldable t, Monad m)   => t a -> (a -> m b) -> m ()

`forM` and `mapM` are interchangeable:

    forM = flip mapM


5.4 `filterM`

    filter :: (a -> Bool) -> [a] -> [a]

the `filterM` from `Control.Monad`, the predicate returns a monadic value whose 
result is a `Bool`, but because it's a monadic value, its context can be anything 
from a possible failure to nonderminism and more.

    firlterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

> keepSmall :: Int -> Writer [String] Bool
> keepSmall x
>   | x < 4 = do
>       tell ["Keeping " ++ show x]
>       return True
>   | otherwise = do
>       tell [show x ++ " is too large, throwing it away."]
>       return False

> small_writer :: Writer [String] [Int]
> small_writer = filterM keepSmall [9, 11, 5, 2, 10, 3, 1]

take the value out:

> small_writer_val = fst $ runWriter $ small_writer

print the log with the `mapM_`


> small_writer_log = mapM_ putStrLn $ snd $ runWriter $ small_writer

A cool Haskell trick by using `filterM` to get the powerset of a list:

powerset - all the combinations of keeping and throwing out elements from a set.

> powerset :: [a] -> [[a]]
> powerset xs = filterM (\x -> [True, False]) xs


5.5 `foldM`

    foldl :: (b -> a -> b) -> b -> [a] -> b

`foldM` does the same thing, except it takes a binary function that produces a 
monadic value and folds the list up with that.

    foldM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
