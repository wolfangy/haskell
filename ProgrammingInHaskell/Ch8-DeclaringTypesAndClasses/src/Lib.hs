module Lib
    ( 
          Assoc(..)
        , find
        , int2bin
        , rmdups
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