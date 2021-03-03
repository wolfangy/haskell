module Lib where

import Data.List

-- 1.
--
half :: Rational -> Rational
half x = x / 2

halfIdentity :: Rational -> Rational
halfIdentity = (*2) . half

-- 2.
--
listOrdered ::  (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where
        go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

-- 3.
--
plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y ==  y + x

-- 4.
--
multiplyAssociative x y z = x * (y * z) == (x * y) * z
multiplyCommutative x y = x * y == y * x

-- 5.
--
quotRemIdentity :: Integral a => a -> a -> a
quotRemIdentity x y = (quot x y) * y + (rem x y)


divModIdentity :: Integral a => a -> a -> a
divModIdentity x y = (div x y) * y + (mod x y)


-- 6.
--
powerAssociate x y z = x ^ (y ^ z) == (x ^ y) ^ z
powerCommutative x y = x ^ y == y ^ x


-- 7.
--
reverseIdentity = reverse . reverse


-- 8.
--
-- f $ a = f a
-- f . g = \x -> f (g x)


-- 9.
--
-- foldr (:) == (++)
-- foldr (++) [] == concat

-- 10.
--
lengthTakeIdentity n xs = length (take n xs)


-- 11.
--
readShowIndentity x = (read (show x))
