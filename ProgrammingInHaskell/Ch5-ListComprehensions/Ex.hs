module Ex where

import Ch5

-- 1.
e1 = sum [x ^ 2 | x <- [1 .. 100]]

-- 2.
grid :: Int -> Int -> [(Int, Int)]
grid w h = [(x, y) | x <- [0 .. w], y <- [0 .. h]]

-- 3.
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4.
replicate' :: Int -> a -> [a]
replicate' n v = [v | _ <- [1 .. n]]

-- 5.
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x ^ 2 + y ^ 2 == z ^ 2]

-- 6.
perfects :: Int -> [Int]
perfects n = [v | v <- [1 .. n], v == (sum . init . factors $ v)]

-- 7.
e7 :: [(Int, Int)]
e7 = [(x, y) | x <- [1, 2], y <- [3, 4]]

e7' = [[(x, y) | y <- [3, 4]] | x <- [1, 2]]

-- 8.
positions'' :: Eq a => a -> [a] -> Int
positions'' x xs = head $ find' x [(v, p) | (v, p) <- zip xs [1 ..]]

-- 9.
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]