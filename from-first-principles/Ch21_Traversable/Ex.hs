module Ex where

import Control.Applicative

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure  = Identity
    Identity f <*> i = fmap f i

instance Foldable Identity where
    foldMap f (Identity a) = f a

instance Traversable Identity where
    traverse f (Identity a)= Identity <$> f a

-- Constant
--

newtype Constant a b = 
    Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
    fmap f (Constant a) = Constant a

instance (Semigroup a) =>  Semigroup (Constant a b) where
    (Constant a) <> (Constant a') = Constant (a <> a')

instance (Monoid a) => Monoid (Constant a b) where
    mempty = Constant $ mempty

instance (Monoid a) => Applicative (Constant a) where
    pure _ = mempty
    Constant a <*> Constant b = Constant $ a <> b

instance Foldable (Constant a) where
    foldMap _ (Constant g) = mempty

instance Traversable (Constant a) where
    traverse _ (Constant g) = pure (Constant g)


-- Maybe
--
data Optional a =
    Nada | Yap a deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yap a) = Yap $ f a

instance Applicative Optional where
    pure = Yap
    Nada <*> _ = Nada
    _ <*> Nada = Nada
    (Yap f) <*> (Yap a) = Yap $ f a

instance Foldable Optional where
    foldMap f Nada = mempty
    foldMap f (Yap a) = f a

    foldr f b Nada = b
    foldr f b (Yap a) = f a b

instance Traversable (Optional) where
    traverse f Nada = pure Nada
    traverse f (Yap a) = Yap <$> f a

-- List
--
data List a =
    Nil
    | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons x xs) = (f x) <> (foldMap f xs)

    foldr f b Nil = b
    foldr f b (Cons x xs) = f x (foldr f b xs)

instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons x xs) = (Cons <$> (f x)) <*> (traverse f xs)

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)


-- Three
--
data Three a b c =
   Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c
    foldr f x (Three a b c) = f c x

instance Traversable (Three a b) where
    traverse f (Three a b c) = (Three a b) <$> f c


-- Pair
--
data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where 
    foldMap f (Pair a b) = f b

    foldr f x (Pair a b) = f b x

instance Traversable (Pair a) where
    traverse f (Pair a b) = (Pair a) <$> f b


-- Big
--
data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
    fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
    foldMap f (Big a b b') = f b <> f b'

    foldr f x (Big a b b') = f b (f b' x)

instance Traversable (Big a) where
    traverse f (Big a b b') = (Big a) <$> f b <*> f b'

-- S
--
data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
    fmap f (S n a) = S (f <$> n) (f a)

instance (Applicative n) => Applicative (S n) where
    pure f = S (pure f) f
    S (f') f <*> S n a = S (f' <*> n) (f a)

instance Foldable n => Foldable (S n) where
    foldMap f (S n a) = foldMap f n <> f a
    foldr f b (S n a) = f a (foldr f b n)

instance (Traversable n) => Traversable (S n) where
    traverse f (S n a) = S <$> (traverse f n) <*> (f a)

-- Tree
--
data Tree a=
    Empty
    | Leaf a
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node t a t') = Node (f <$> t) (f a) (f <$> t')

instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node t a t') = foldMap f t <> f a <> foldMap f t'

instance Traversable Tree where
    traverse f Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node t a t') = Node <$> (traverse f t) <*> (f a) <*> (traverse f t')

