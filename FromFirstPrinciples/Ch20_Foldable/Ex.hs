module Ex where

import Data.Monoid

-- foldMap :: (Monoid m) => (a -> m) -> t a -> m
--

-- 1.
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0
sum' t = foldMap Sum t

-- 2.
product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1
product' t = foldMap Product t

-- 3.
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x t = getAny $ foldMap (\a -> Any(a == x)) t
elem' x = foldr (\a b -> b || (a == x)) True


-- 4.
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr
    (\a b -> 
        case b of Nothing -> Just a
                  (Just y) -> if a > y 
                              then Just y 
                              else Just a)
    Nothing

-- 5.
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr
    (\a b -> 
        case b of
            Nothing -> Just a
            (Just y) -> if a > y
                        then Just a
                        else Just y)
    Nothing

-- 6.
null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

-- 7.
length :: (Foldable t) => t a -> Int
length = foldr (\a b -> b + 1) 0

-- 8.
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

-- 9.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap (<> mempty)

-- 10.
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> b <> (f a)) mempty

-- 11.
foldr' :: (Monoid b, Foldable t) => (a -> b -> b) -> b -> t a -> b
foldr' f b ta = foldMap (\a -> f a b) ta


-- 1.
--
data Constant a b = Constant b

instance Foldable (Constant a) where
    foldr f b (Constant a) = f a b
    foldMap f (Constant b) = f b

-- 2.
--
data Two a b = Two a b

instance Foldable (Two a) where
    foldr f b (Two x y) = f y b
    foldMap f (Two x y) = f y

-- 3.
--
data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldr f b (Three x y z) = f z b
    foldMap f (Three x y z) = f z

--4.
--
data Three' a b = Three' a b b

instance Foldable (Three' a) where
    foldr f b (Three' x y y') = f y b
    foldMap f (Three' x y y') = f y <> f y'


-- filter function for Foldable types
filterF :: (Applicative f, Foldable t, Monoid (f a))
    => (a -> Bool) -> t a -> f a
filterF f ta =
    foldMap (\a -> if f a then pure a else mempty) ta

