module Ex where

import Data.Functor
import Control.Applicative
import Control.Monad

j :: Monad m => m (m a) -> m a
j = join

j' :: Monad m => m (m a) -> m a
j' mma = mma >>= (\ma -> ma)


l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l1' :: Monad m => (a -> b) -> m a -> m b
l1' f ma = pure f <*> ma

l1'' :: Monad m => (a -> b) -> m a -> m b
l1'' f ma = ma >>= (\a' -> return $ f a')

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2' f ma mb = ma >>= (\a -> mb >>= (\b -> return $ f a b))

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

a' :: Monad m => m a -> m (a -> b) -> m b
a' ma mf = do
    a <- ma
    f <- mf
    return $ f a

a'' :: Monad m => m a -> m (a -> b) -> m b
a'' ma mf = ma >>= (\a -> mf >>= (\f -> return $ f a))

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f = do
    b <- (f x)
    bs <- meh xs f
    return (b : bs)

meh' :: Monad m => [a] -> (a -> m b) -> m [b]
meh' [] f = return []
meh' (x: xs) f = (:) <$> (f x) <*> (meh' xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType [] = return []
flipType (ma:mas) = do
    a <- ma
    as <- flipType mas
    return (a : as)

flipType' :: (Monad m) => [m a] -> m [a]
flipType' mas = meh mas id
