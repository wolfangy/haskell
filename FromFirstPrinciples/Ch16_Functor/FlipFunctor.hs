{-# LANGUAGE FlexibleInstances #-}

module FlipFunctor where

data Tuple a b =
    Tuple a b deriving (Eq, Show)

instance Functor (Tuple a) where
    fmap f (Tuple a b) = Tuple a $ f b

newtype Flip f a b =
    Flip (f b a) deriving (Eq, Show)

instance Functor (Flip Tuple b) where
    fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

flippedTuple = fmap (+1) (Flip (Tuple 1 "blahblah"))


