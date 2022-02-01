module WordNumber where

import Data.List

digitToWord :: Int -> String
digitToWord n
    | n == 0 = "zero"
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eight"
    | n == 9 = "nine"
    | otherwise = "-"


digits :: Int -> [Int]
digits n = go n []
    where
        go :: Int -> [Int] -> [Int]
        go n arr
            | n < 10 = (:) n arr
            | otherwise = go (n `div` 10) ((: arr) . (`rem` 10) $ n)

wordNumber :: Int -> String
wordNumber n = concat . (intersperse "-") . (map digitToWord) . digits $ n

