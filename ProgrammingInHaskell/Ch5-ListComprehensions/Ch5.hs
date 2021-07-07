module Ch5 where

import Data.Char
import Data.List

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
shift deta c
    | isLower c = let v = letter2int c
                      nv =((v + deta) `mod` 26)
                  in  int2letter nv
    | otherwise = c

encode :: Int -> String -> String
encode deta xs = [shift deta x | x <- xs]

-- Cracking the cipher
table :: [Float]
table = [
    8.1 {-a-}, 1.5{-b-}, 2.8{-c-}, 4.2{-d-},
    12.7{-e-}, 2.2{-f-}, 2.0{-g-}, 6.1{-h-},
    7.0 {-i-}, 0.2{-j-}, 0.8{-k-}, 4.0{-l-},
    2.4 {-m-}, 6.7{-n-}, 7.5{-o-}, 1.9{-p-},
    0.1 {-q-}, 6.0{-r-}, 6.3{-s-}, 9.0{-t-},
    2.8 {-u-}, 1.0{-v-}, 2.4{-w-}, 0.2{-x-},
    2.0 {-y-}, 0.1{-z-}]

percent :: Int -> Int -> Float
percent x y = (fromIntegral x / fromIntegral y) * 100

freqs :: [Char] -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
    where n = lowers xs

chiSqr :: [Float] -> [Float] -> Float
chiSqr os es = sum [((o - e)^2)/e | (o, e) <- zip os es ]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

chitab' xs = [chiSqr (rotate n $ freqs xs) table | n <- [0..25]]
 
factors' tab = sort tab

crack :: String -> String
crack xs = encode (-factor) xs
    where
        factor = head (positions (minimum chitab) chitab)
        chitab = [chiSqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs
