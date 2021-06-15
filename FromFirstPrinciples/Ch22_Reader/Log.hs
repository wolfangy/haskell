module Log where

import Control.Monad.Trans.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a * b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b 
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

instance Semigroup (DiffList a) where
    (DiffList a) <> (DiffList b) = DiffList $ a . b

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)

toDiffList :: [a] -> DiffList a
toDiffList arr = DiffList $ (\xs -> arr ++ xs)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

gcd'' :: Int -> Int -> Writer (DiffList String) Int
gcd'' a b 
    | b == 0 = do
        tell (toDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcd'' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result
