module Ex where

n = a `div` length xs
    where
        a = 10
        xs = [1..5]

last1 :: [a] -> [a]
last1 [] = []
last1 [x] = [x]
last1 (x:xs) = last1 xs

last2 :: [a] -> [a]
last2 arr = [arr !! (length arr - 1)]

last3 :: [a] -> [a]
last3 arr = drop (length arr - 1) arr

last4 :: [a] -> [a]
last4 arr = [snd p | p <- zip [0..] arr, fst p == length arr - 1]

last5 = take 1 . reverse

init1 :: [a] -> [a]
init1 arr = take (length arr - 1) arr

init2 :: [a] -> [a]
init2 = reverse . tail . reverse

