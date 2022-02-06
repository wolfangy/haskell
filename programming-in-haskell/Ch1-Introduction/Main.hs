module Main where

qsort :: Ord a => [a] -> [a]
qsort []  = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger  = [a | a <- xs, a > x]

qsortUniq :: Ord a => [a] -> [a]
qsortUniq []  = []
qsortUniq (x:xs) = qsortUniq smaller ++ [x] ++ qsortUniq larger
    where
        smaller = [a | a <- xs, a < x]
        larger  = [a | a <- xs, a > x]

qsortReverse :: Ord a => [a] -> [a]
qsortReverse [] = []
qsortReverse (x:xs) = qsortReverse larger ++ [x] ++ qsortReverse smaller
    where 
        smaller = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]

seqn :: (Monad m) => [m a] -> m [a]
seqn [] = return []
seqn (a:as) = do
    x <- a
    xs <- seqn as
    return (x:xs)

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product xs

main :: IO ()
main = undefined