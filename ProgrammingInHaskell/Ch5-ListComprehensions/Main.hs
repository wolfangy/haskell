module Main where

import Data.Char

b1 = [x ^ 2 | x <- [1 .. 5]]

b2 = [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]

b3 = [(x, y) | x <- [1 .. 3], y <- [x .. 3]]

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

firsts' :: [(a, b)] -> [a]
firsts' xs = [a | (a, _) <- xs]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

-- Guards

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [1 .. n], prime x]

find' :: Eq a => a -> [(a, b)] -> [b]
find' k map = [b | (a, b) <- map, a == k]

-- zip function
z1 = zip ['a', 'b', 'c'] [1..4]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sortted :: Ord a => [a] -> Bool 
sortted xs = and [a <= b| (a, b)<-pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions v xs = [ i | (i, x) <- zip [0..] xs, v == x]

-- String
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [ 1 | x' <- xs, x' == x]


-- Caesar cipher

letter2int :: Char -> Int
letter2int ch = ord ch - ord 'a'

int2letter :: Int -> Char 
int2letter v = chr (ord 'a' + v)

shift :: Int -> Char -> Char
shift deta ch =
    let
        v = letter2int ch
        nv = v + deta
        nv' = if nv >= 26 then nv `mod` 26 else nv
    in
        int2letter nv'

main :: IO ()
main = undefined