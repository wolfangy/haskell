module Guard_Ex where


avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.6 = 'D'
    | otherwise = 'F'
    where y = x / 100

-- 1.
avgGrade' :: (Fractional a, Ord a) => a -> Char
avgGrade' x
    | otherwise = 'F'
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.6 = 'D'
    where y = x / 100

-- 2.
avgGrade'' :: (Fractional a, Ord a) => a -> Char
avgGrade'' x
    | y >= 0.7 = 'C'
    | y >= 0.9 = 'A'
    | y >= 0.6 = 'D'
    | y >= 0.8 = 'B'
    | otherwise = 'F'
    where y = x / 100


-- 3.

pal :: (Eq a) => [a]-> Bool
pal xs
    | xs == reverse xs = True
    | otherwise = False


-- 6.

number :: (Num a, Ord a) => a -> a
number x
    | x < 0     = -1
    | x == 0    = 0
    | x > 0     = 1
