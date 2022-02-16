{-# LANGUAGE GADTs #-}

module Lib
    ( 
          Assoc(..)
        , Nat (..)
        , add
        , find
        , int2bin
        , rmdups
        , intToNat
        , natToInt
    ) where

type Assoc k v = [(k, v)]

find :: Eq a => a -> Assoc a b -> Maybe b
find _ [] = Nothing
find k ((key, val):xs) = if k == key
                            then Just val
                            else find k xs

int2bin :: Int -> [Int]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

rmdups' :: Eq a => [a] -> [a]
rmdups' = foldr (\a b -> a : filter (/=a) b) []


data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat
  deriving Show

add :: Nat -> Nat -> Nat
add Zero n  = n
add (Succ m) n = Succ $ add m n

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Succ $ intToNat (n - 1)

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n)  = 1 + natToInt n