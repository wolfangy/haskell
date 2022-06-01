> {-# LANGUAGE GADTs #-}
> import Data.Foldable

14.1 Monoid

In mathematics, a `monoid` is a set together with an associative operator that 
combines two elements from the set, and an identity element for the operator.

    class Monoid a where
        mempty :: a
        mappend :: a -> a -> a

        mconcat :: [a] -> a
        mconcat = foldr mappend mempty

`Monoid` class are required to satisfy the following identity and associative laws:

    mempty `mappend` x = x
    x `mappend` mempty = x

    x `mappend` (y `mappend` z) = (x `mappend` y) `mappend` z

Attempting to decalre two separate instances for `Monoid Int` will result int an 
error.

The solution is to introduce special-purpose wrapper types for each of the two 
instances.

    newtype Sum a = Sum a
        deriving (Eq, Ord, Show, Read)

    getSum :: Sum a -> a
    getSum (Sum x) = x

    instance Num a => Monoid (Sum a) where
        mempty = 0
        Sum x `mappend` Sum y = Sum (x + y)
    
    newtype Product a = Product a
        deriving (Eq, Ord, Show, Read)
    
    getProduct :: Product a -> a
    getProduct (Product x) = x

    instance Num a => Monoid (Product a) where
        mempty = Product 1
        Product x `mappend` Product y = Product (x * y)

14.2 Foldables

One of the primary applications of monoids in Haskell is to combine all the values 
in a data structure to give a single value.

Data.Foldable:

    class Foldable t where
        fold :: Monoid a => t a -> a
        foldMap :: Monoid b => (a -> b) -> t a -> b
        foldr   :: (a -> b -> b) -> b -> t a -> b
        foldl   :: (b -> a -> b) -> b -> t a -> b

by convention foldable types are usually denoted by `t`

`fold` takes a data structure of type `t a` whose elements have type `a`, and combines 
the elements using the monoid primitives for this type to give a single value of 
type `a`

`foldMap` generalises `fold` by taking a function of type `a -> b` as an additional 
argument, which is applied to each element in the structure prior to combining the 
resulting values using the monoid primitives for the type `b`.

`foldr` and `foldl` there is NO need to have an underlying monoid, because a starting 
value and function to combine two values are explicitly supplied as arguments.

> data Tree a where
>   Leaf :: a -> Tree a
>   Node :: Tree a -> Tree a -> Tree a
>   deriving Show

> instance Foldable Tree where
>   fold (Leaf x) = x
>   fold (Node l r) = fold l `mappend` fold r

>   foldMap f (Leaf x) = f x
>   foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

>   foldr f v (Leaf x) = f x v
>   foldr f v (Node l r) = foldr f (foldr f v l) r

>   foldl f v (Leaf x) = f v x
>   foldl f v (Node l r) = foldl f (foldl f v l) r

> toTreel :: [a] -> Tree a
> toTreel [x] = Leaf x
> toTreel (x:xs) = foldl (\t v -> Node t (Leaf v)) (Leaf x) xs


Other primitives and defaults:

    null :: t a -> Bool

    length :: t a -> Int

    elem :: Eq a => a -> t a -> Bool

    maximum :: Ord a => t a -> a

    minimum :: Ord a => t a -> a

    sum :: Num a => t a -> a

    product :: Num a => t a -> a

The class also includes versions of `foldr` and `foldl` for structures that contain 
at least one element, and hence do not require a starting value:

    foldr1 :: (a -> a -> a) -> t a -> a 
    foldl1 :: (a -> a -> a) -> t a -> a

The final primitive in the class flattens a data structure to a list

    toList :: t a -> [a]

The three default definitions in the foldable class establish important relationships 
between the primitives `fold`, `foldMap`, and `toList`

    fold        = foldMap id

    foldMap f   = foldr (mappend . f) mempty

    toList      = foldMap (\x -> [x])

`fold` can be viewed as a special case of `foldMap` where the `id` function is applied 
to each element prior to combining them.

`foldMap` can be defined in terms of `foldr` by applying the function `f` to each 
element before they are combined using the `monoid` primitives.

`toList` can be defined in terms of `foldMap` by first transforming each element 
into a singleton list, and then concatenating the resulting list using the list monoid.


14.3 Traversable

the idea of mapping a function over each element of a data structure:

    class Functor f where
        fmap :: (a -> b) -> f a -> f b


However the idea of mapping a function over a list can be generalised further.

suppose that the function `g` that is applied to each element may fail, in the 
sense that is has type `a -> Maybe b`, and the mapping as a whole only succeeds 
if every such application succeeds.

    traverse :: (a -> Maybe b) -> [a] -> Maybe [b]
    traverse g [] = pure []
    traverse g (x:xs) = pure(:) <*> g x <*> traverse g xs

The idea of traversing a data structure in the above manner isn't specific to the 
type of the lists, and isn't specific to argument functions that may fail.

The class of types that support such a generalised mapping function are called:
`traversable types` or `traversables` for short.

    class (Functor t, Foldable t) => Traversable t where
        traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

`t` is both functorial and foldable to be an instance of the class `Traversable`

The requirement that `t` is `foldable` ensures that values in a traversable type 
can also be folded up if desired.

> instance Functor Tree where
>   fmap f (Leaf x) = Leaf $ f x
>   fmap f (Node l r) = Node (fmap f l) (fmap f r)

> instance Traversable Tree where
>   traverse g (Leaf x)   = fmap Leaf (g x)
>   traverse g (Node l r) = Node <$> traverse g l <*> traverse g r

> dec :: Int -> Maybe Int
> dec n = if n > 0 then Just $ n - 1 else Nothing

Other primitives and defaults

`sequenceA`:

    sequenceA :: Applicative f => t (f a) -> f (t a)

`sequenceA` transforms a data structure whose elements are applicative actions into 
a single such action that returns a data structure.

    sequenceA := traverse id

    traverse g := sequenceA . fmap g

`traverse` a data structure using an effecful function, we can first apply the 
function to each elelment using `fmap`, and then combine all the effects using 
`sequenceA`

!!! it is generally preferable to define `traverse` rather than `sequenceA`.
As `sequenceA` makes two passes over the data structure, one using the `fmap` and 
one using `sequenceA`.

`mapM` and `sequence`:

the class also provides two traversable primitives for the special case when the 
effects that are involved are monadic rather than applicative.

    mapM     :: Monad m => (a -> m b) -> t a -> m (t b)
    
    sequence :: Monad m => t (m a) -> m (t a)

    mapM := traverse

    sequence := sequenceA

Exercise:

1. Make a Pair type into a monoid

> data Pair a b where
>   Pair :: a -> b -> Pair a b
>   deriving Show

> instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
>   (Pair x1 y1) <> (Pair x2 y2) = Pair (x1 <> x2) (y1 <> y2)

> instance (Monoid a, Monoid b) => Monoid (Pair a b) where
>   mempty = Pair mempty mempty
>   mappend = (<>)

2. Make a function type `a -> b` into a monoid:

> data Fn a b where
>   F :: (a -> b) -> Fn a b

> runFn :: Fn a b -> a -> b
> runFn (F f) = f

> instance (Semigroup b) => Semigroup (Fn a b) where
>   (F f) <> (F g) =F $ \x -> (f x) <> (g x)

> instance (Monoid b) => Monoid (Fn a b) where
>   mempty = F (\x -> mempty)
>   mappend = (<>)

3. Make `Maybe` type can be made foldable and traversable

> data Option a where
>   None :: Option a
>   Some :: a -> Option a
>   deriving Show

> instance Foldable Option where
>   fold None = mempty
>   fold (Some v) = v

>   foldMap f None = mempty
>   foldMap f (Some v) = f v
