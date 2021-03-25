module MoreStruct where

data Wrap f a =
    Wrap (f a) deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)

wrappedJust2 = fmap (+1) (Wrap (Just 1))

wrappedArr = fmap (+1) (Wrap [1, 2, 3])

