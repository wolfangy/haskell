module Ex where

import Control.Applicative
import Data.Maybe

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' key [] = Nothing
lookup' key ((a, b):xs) = if key == a then Just b else lookup key xs

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z


zs :: Maybe Integer
zs = lookup 4 $ zip x y

zs' :: Integer -> Maybe Integer
zs' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> zs' <*> zs'

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b)= f a b

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

fromMaybe :: a -> Maybe a -> a
fromMaybe a (Just a') = a'
fromMaybe a Nothing = a

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' = summed <$> ((,) <$> xs <*> ys)

main :: IO()
main = do
    print $ sequenceA [Just 3, Just 2, Just 1]

    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> zs)

    print $ bolt 7
    print $ fmap bolt z

    print $ sequenceA [(>3), (<8), even] 7



