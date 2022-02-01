module Main where

qsort :: Ord a => [a] -> [a]
qsort []  = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger  = [a | a <- xs, a > x]

seqn :: (Monad m) => [m a] -> m [a]
seqn [] = return []
seqn (a:as) = do
    x <- a
    xs <- seqn as
    return (x:xs)

main :: IO ()
main = undefined