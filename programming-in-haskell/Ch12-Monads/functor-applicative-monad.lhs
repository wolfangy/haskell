> {-# LANGUAGE GADTs #-}
> import Data.Functor
> import Data.Char
> import Prelude hiding (mapM)

Monads and more

12.1 Functors

class Functor f where

    fmap :: (a -> b) -> f a -> f b

`f` must be a parameterised type


    instance Functor [] where
        fmap = map

> data Tree a where
>   Leaf :: a -> Tree a
>   Node :: Tree a -> Tree a -> Tree a
>   deriving Show

> instance Functor Tree where
>   fmap g (Leaf x) = Leaf (g x)
>   fmap g (Node l r) = Node (fmap g l) (fmap g r)

In the sense that `f a` is a data structure that contains elements of type a,
which is sometimes called a container type.

fmap applies a given function to each such element. 

!!! However not all instance fit this pattern, for example, IO type is not
a container type in normal sense of the term because its values represnet
input/output actions whose internal structure we do not have access to.

    instance Functor IO where
        fmap g mx = do
            x <- mx
            return (g x)

Two key benefits of using functors:

1. the function `fmap` can be used to process the elements of any structure that
is functorial.

2. We can define generic functions that can be used with any functor.

exp: 

> inc :: Functor f => f Int -> f Int
> inc = fmap (+1)

> just2 = inc (Just 1)

    Just 2

> from2to6 = inc [1..5]

    [2, 3, 4, 5, 6]

Functor laws:

1. fmap preserves the identity function:

    fmap id = id

left side:

    id :: a -> a

    fmap id :: f a -> f a

right side:

    id :: f a -> f a

2. fmap also preserve functions composition:

    fmap (g . h) = fmap g . fmap h

left side:

    g :: b -> c 
    h :: a -> b
    fmap (g . h) :: f a -> f c

right side:

    fmap h :: f a -> f b
    fmap g :: f b -> f c

!!! The infix version of `fmap` : "<\$>"

12.2 Applicatives

    pure :: a -> f a

    (<*>) :: f (a -> b) -> f a -> f b

`pure` converts a value of type `a` into a structure of type `f a`

`<*>` is a generalised form of function application for which the argument function, 
the argument value, and the result value are all contained in `f`
structures.

a typical use of pure and <*> has the following form:

    pure g <*> x1 <*> x2 <*> ... <*> xn

`g` is a curried function that takes n arguments of type `a1 ... an` and produces
a result of type `b`.

However in applicative, each argument `xi` has type `f ai` rather than just `ai`

The class of functors that suppor the `pure` and `<*>` functions are called 
`applicative functors` or `applicatives`.

    class Functor f => Applicative f where
        pure  :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b

    instance Applicative [] where
        pure x = [x]

        gs <*> xs = [g x | g <- gs, x <- xs]


    instance Applicative IO where
        pure = return

        mg <*> mx = do
            g <- mg
            x <- mx
            return (g x)


> getChars :: Int -> IO String
> getChars 0 = return []
> getChars n = pure (:) <*> getChar <*> getChars (n - 1)

1. Applicatives was the desire the generalise the idea of mapping to functions
with multiple arguments.

2. Applicative functor can also be viewed as abstracting the idea of applying
pure functions to effectful arguments, with the precise form of effects that
permitted depending on the nature of the underlying functor.

3. Using applicatives also has the important benefit that we can define generic 
functions that can be used with any applicative functor.

> sequence' :: Applicative f => [f a] -> f [a]
> sequence' [] = pure []
> sequence' (x:xs) = pure (:) <*> x <*> sequence' xs

(:) :: a -> [a] -> [a]
pure (:) :: f a -> f [a] -> f [a]
x :: f a
xs :: [f a]

> getChars' :: Int -> IO String 
> getChars' n = sequence' (replicate n getChar)

replicate n getChar :: [IO Char]

Applicative laws

1. `pure` preserves the identity function:

pure id <*> x = x0

2. `pure` also preserves function application:

pure (g x) = pure g <*> pure x

3. when an effectful function is applied to a pure argument, the order in which 
we evaluate the two components doesn't matter:

x <*> pure y = pure (\ g -> g y) <*> x

4. modulo the types that are involved, the operator `<*>` is associative:

x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

Shows how `fmap` can be defined in terms of the two applicative primitives:

fmap g x = pure g <*> x

g <\$> x1 <*> x2 <*> ... <*> xn <==> pure g <*> x1 <*> x2 <*> ... <*> xn


12.3 Monads

‼ The applicative style restricts us to applying pure functions to effectful 
arguments.

`>>=` operator is often called `bind`, because the second argument binds the 
result of the first.

Generalising from the expressions that are built by using the `>>=` 
operator has the following structure:

    m1 >>= \ x1 ->
    m2 >>= \ x2 ->
    .
    .
    .
    mn >>= \ xn ->
    f x1 x2 ... xn

The definition of the `>>=` operator ensures that such an expression only succeeds 
if every component `mi` in the sequence successds.

Haskell provides a special notation for expression of the above form, allowing 
them to be written in a simpler manner as follows:

    do
        x1 <- m1
        x2 <- m2
        .
        .
        .
        xn <- mn
        f x1 x2 ... xn

The `do` notation is not specific to the type `IO` and `Maybe`, but can be used 
with any applicative type that forms a monad.

    class Applicative m => Monad m where
        return :: a -> m a
        (>>=)  :: m a -> (a -> m b) -> m b

    return = pure

!!! Unlike the other monads, for the `IO` type, the `return` and `>>=` are 
built-in to the language, ranther that being defined within Haskell itself.


The `state` monad

> type State = Int

The simplest state: one integer


    type ST = State -> State

the most basic form of function on the `State` is a `state transformer`,
abbreviated by `ST`, which takes an input state as its argument and produces
an output state as its result, in which the output state reflects any updates
that were made to the state by the function during the execution.


We may wish to return a result value in addition to updating the state.

For example, if the state represents a counter, a function for incrementing
the counter may also wish to return its courrent value.

We generalise the type of state transformer to also return a result value, with
the type of such values being a parameter of the ST type:

    type ST a = State -> (a, State)


Conversely, a state transformer may also wish to take a argument value. However,
this is no need to further generalise the `ST` type, this behavior can be achieved
by exploiting currying.

For example:    Char -> ST Int
which abbreviates the curried function type:
    Char -> State -> (Int, State)

‼ `type` mechanism cannot be made into instances of classes.

> newtype ST a = S (State -> (a, State))

`S` is introduced as a dummy constructor.

It is convenient to define a special-purpse application function for this type 
which simply removes the dummy constructor:

> app :: ST a -> State -> (a, State)
> app (S st) x = st x

make type `ST` into a monad, first need to turn it into a Functor

> instance Functor ST where
>   fmap g st = S (\ s -> let (x, s') = app st s in (g x, s'))

`let` is similar to the `where`, except that it allows local definitions
to be made at the level of expressions rather than at the level of function 
definition.

> instance Applicative ST where
>   -- pure :: a -> ST a
>   pure x = S (\ s -> (x, s))
>   -- (<*>) :: ST (a -> b) -> ST a -> ST b
>   stf <*> stx = S (\ s ->
>       let (f, s') = app stf s
>           (x, s'') = app stx s'
>       in (f x, s''))

> instance Monad ST where
>   -- (>>=) :: ST a -> (a -> ST b) -> ST b
>   st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')


Relabelling trees 

> tree :: Tree Char
> tree = Node (Node (Leaf 'a') (Leaf 'b')) (Node (Leaf 'c') (Leaf 'd'))

> labelR :: Tree a -> Int -> (Tree Int, Int)
> labelR (Leaf _) n = (Leaf n, n + 1)
> labelR (Node l r) n = (Node l' r', n'')
>   where
>       (l', n') = labelR l n
>       (r', n'') = labelR r n'

> fresh :: ST Int
> fresh = S (\n -> (n, n + 1))

> labelA :: Tree a -> ST (Tree Int)
> labelA (Leaf _) = Leaf <$> fresh
> labelA (Node l r) = Node <$> labelA l <*> labelA r

> labelM :: Tree a -> ST (Tree Int)
> labelM (Leaf _) = do
>       n <- fresh
>       return $ Leaf n
> labelM (Node l r) = do
>       l' <- labelM l
>       r' <- labelM r
>       return $ Node l' r'

Generic functions

> mapM :: Monad m => (a -> m b)  -> [a] -> m [b]
> mapM f [] = return []
> mapM f (x:xs) = do
>       b  <- f x
>       bs <- mapM f xs
>       return $ b : bs 

> conv :: Char -> Maybe Int
> conv c
>    | isDigit c = Just (digitToInt c)
>    | otherwise = Nothing

> filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
> filterM _ [] = return []
> filterM f (x:xs) = do
>   r <- f x
>   xs' <- filterM f xs
>   return $ if r then (x:xs') else xs'

> join :: Monad m => m (m a) -> m a
> join mmx = do
>      mx <- mmx
>      x <- mx
>      return x

Monad laws:

1. If we `return` a value and then feed this into a monadic
function, this should give the same result as simply applying 
the function to the value:

    return x >>= f      = f x

2. If we feed the result of a monadic computation into the 
function `return`, this should give the same result as simply 
performing the computation:

    mx >>= return       = mx

3. `>>=` is associative

    (mx >>= f) >>= g    = mx >>= (\x -> (f x >>= g))

!!! the right-hand side of the equation cannot just write as:
    mx >>= (f >>= g), which is not type correct.