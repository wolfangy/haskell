module Ch4_Exercise where

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- 1. 
--
length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + (length' xs)

-- 4. Fix error: 6 / length' [1, 2, 3]
--
-- Reason: (/) :: Fractional a => a -> a -> a
-- length' :: [a] -> Integer
-- Integer is not instance of Fractional

-- fix 1:
result1 = 6 `div` length' [1, 2, 3]

-- fix 2:
result2 = 6 / (fromIntegral $ length' [1, 2, 3])::Float

-- 8.
-- 
isParlindrome :: (Eq a) => [a] -> Bool
isParlindrome str = (==) str $ reverse str

-- 9.
--
myAbs :: Integer -> Integer
myAbs num = if num >= 0 then num else (negate num)

-- 10.
--
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f left right = ((snd left, snd right), (fst left, fst right))

f' :: (a, b) -> (c, d) -> ((b, d), (a, c))
f' (lf, ls) (rf, rs) = ((ls, rs), (lf, rf))

-- 11.
--
x = (+)
f11 :: [a] -> Int
f11 xs = x w 1
    where w = length xs

-- 12
--
id :: a -> a
id x = x

id' = (\x -> x)

-- 13.
--
fst' :: (a, b) -> a
fst' (a, b) = a


