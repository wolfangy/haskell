module List where

import Data.Semigroup
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
    Nil <> t = t
    t <> Nil = t
    (Cons x xs) <> ys = Cons x (xs <> ys)

instance Monoid (List a) where
    mempty = Nil

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    -- <*>:: List(a -> b) -> List a -> List b
    --(<*>) (Cons f fs) a= fmap f a <> (fs <*> a)
    (<*>) fs xs = flatMap (flip fmap xs) fs

append :: List a -> List a -> List a
append = (<>)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List(List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

toList:: Foldable t => t a -> List a
toList = foldr Cons Nil

fromList :: List a -> [a]
fromList = fold (:) []

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x $ take' (n-1) xs

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where
            xs' = let (ZipList' l) = xs
                  in take' 3000 l
            ys' = let (ZipList' l) = ys
                  in take' 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Semigroup (ZipList' a) where
    (<>) (ZipList' Nil) rs = rs
    (<>) ls (ZipList' Nil) = ls
    (<>) (ZipList' ls) (ZipList' rs) = ZipList' $ ls <> rs

instance Applicative ZipList' where
    pure x = ZipList' $ Cons x Nil
    (<*>) (ZipList' Nil) _ = ZipList' Nil
    (<*>) _ (ZipList' Nil) = ZipList' Nil
    (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons x xs)) =
        (ZipList'$ Cons (f x) Nil) <> ((ZipList' fs) <*> (ZipList' xs))
