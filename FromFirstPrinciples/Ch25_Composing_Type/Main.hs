{-# LANGUAGE InstanceSigs #-}
module Main where

-- A monad transformer is a variant of an ordinary type that takes an additional
-- type argument that is assumed to have Monad instance.
--
-- The transformer variant of a type gives us a Mona instance that binds over both
-- bits of structure. This allows us to compose monads and combine their effects.
--
newtype Identity a =
    Identity { runIdentity :: a }

newtype Compose f g a =
    Compose { getCompose :: f(g a) }
    deriving (Eq, Show)

compMaybeArr = Compose [Just 1, Nothing]
-- f ~ []
-- g ~ Maybe
-- a ~ Int

valCompMaybeArr = getCompose compMaybeArr

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

compMaybeArr' = fmap (+1) compMaybeArr

-- The composition of two datatypes that have a Functor instance gives rise to a
-- new Functor instance.

-- fucntors are closed under composition

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose (pure (pure a))

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose fg_a_b) <*> (Compose fga) = Compose $ (<*>) <$> fg_a_b <*> fga

-- <*> :: g (a -> b) -> (g a -> g b)
-- <$> :: (a -> b) -> f a -> f b
-- fg_a_b :: f (g(a -> b))
-- (<*>) <$> f(g(a -> b)) :: f(g a -> g b)

instance (Foldable f, Foldable g) => Foldable (Compose f g ) where
    foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
    foldMap f (Compose fga) = (foldMap . foldMap) f fga 


instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse :: Applicative m => (a -> m b) -> Compose f g a -> m (Compose f g b)
    traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga


class Bifunctor p where
    bimap ::  (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a-> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

data Deux a b = Deux a b deriving (Eq, Show)

instance Bifunctor Deux where
    bimap f g (Deux a b) = Deux (f a) (g b)


-- Const
data Const a b = Const a deriving (Eq, Show)

instance Bifunctor Const where
    bimap f g (Const a) = Const (f a)

-- Drei

data Drei a b c = Drei a b c deriving (Eq, Show)

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)

--data Either a b = Left a | Right b deriving (Eq, Show)

instance Bifunctor Either where
    bimap f _ (Left a) = Left $ f a
    bimap _ g (Right a) = Right $ g a


newtype IdentityT f a =
    IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity $ f a

instance (Applicative m) => Applicative (IdentityT m) where
    pure = IdentityT . pure
    (IdentityT mf) <*> (IdentityT ma) = IdentityT $ mf <*> ma


instance Monad Identity where
    return = pure
    (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
    return = pure

    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT ma) >>= fmb = IdentityT $ ma >>= runIdentityT . fmb 

main :: IO ()
main = undefined
