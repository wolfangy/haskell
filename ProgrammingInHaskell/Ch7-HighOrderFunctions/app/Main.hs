module Main where

import Data.Char
import Lib
import Vote
import Ex (all')

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]


main :: IO ()
main = do
    print $ map' (+1) [1, 3, 5, 7]
    print $ filter' even [1..9]
    print $ all' even [2, 4, 6, 8]