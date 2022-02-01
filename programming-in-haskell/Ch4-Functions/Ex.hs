module Ex where

-- 1.
halve :: [a] -> ([a], [a])
halve xs =  let l = length xs
                h = l `div` 2
            in
                if h > 0
                then splitAt h xs
                else ([], [])


-- 2.
third :: [a] -> [a]
third []        = []
third [_]       = []
third [_,_]     = []
third [_,_,x,_] = [x]

third' :: [a] -> [a]
third' xs = [xs!!2 | length xs >=2]

third'' :: [a] -> [a]
third'' = (:[]) . head . tail . tail

-- 3.
safetail :: [a] -> [a]
safetail xs
    | null xs = []
    | otherwise = tail xs

safetail' :: [a] -> [a]
safetail' []     = []
safetail' (_:xs) = xs


-- 4.
(||) :: Bool -> Bool  -> Bool
False || False  = False
False || True   = True
True  || False  = True
True  || True   = True

-- 5.
(&&) :: Bool -> Bool -> Bool
(&&) l r =
    if l == True
        then
            if r == True
                then True
                else False
        else False

-- 6.
(&|) :: Bool -> Bool -> Bool
(&|) ctrl val =
    if ctrl
        then val
        else False

-- 7.
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z


-- 8.

luhnDouble :: Int -> Int
luhnDouble n = if n * 2 > 9 
    then n * 2 - 9
    else n *2

luhn :: Int -> Int -> Int -> Int -> Bool 
luhn v1 v2 v3 v4=
    let total  = sum [luhnDouble v1, v2, luhnDouble v3, v4]
    in
        total `mod` 10 == 0