module Ch9_ex where

import Data.Char

-- Data.Char
--
hello = filter isUpper $ "HbEfLrLxO"

upperChar :: String -> String
upperChar [] = []
upperChar (x:xs) = toUpper(x) : xs

upperSentense :: String -> String
upperSentense [] = []
upperSentense (x:xs) = toUpper(x) : upperSentense xs


head' :: [a] -> Maybe a
head' [] = Nothing
head' (x: _) = Just x

fstUpperChar :: String -> Char
fstUpperChar = toUpper . head

-- Ciphers
--
-- chr :: Int -> Char
-- associate a Char with its Int representation in the Unicode system
--
-- ord :: Char -> Int
-- reverse the process of `chr'

shift :: Int -> Char -> Char
shift 0 input = input
shift count ch
    | ch >= 'a' && ch <= 'z' = chr (mod ((ord ch) - smA + count) 26 + smA)
    | ch >= 'A' && ch <= 'Z' = chr (mod ((ord ch) - bgA + count) 26 + bgA)
    | ch == ' ' = ' '
    | otherwise = chr . (+ count) .ord $ ch
    where
        smA = ord 'a'
        smZ = ord 'z'
        bgA = ord 'A'
        bgZ = ord 'Z' 

caesar :: Int -> String -> String
caesar count message = map (shift count) message

unCaesar :: Int -> String -> String
unCaesar count message = map unshift message
    where unshift = shift (- count)

-- Standard Functions
--

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr(xs)


myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny predicate (x:xs) = predicate x || myAny predicate xs


myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem elem (x:xs) = (elem == x) || myElem elem xs


myElem' :: Eq a => a -> [a] -> Bool
myElem' elem arr = myAny (\x -> x == elem) arr

myReverse :: [a] -> [a]
myReverse arr = go arr []
    where
        go [] result = result
        go (x:xs) result = go xs (x:result)

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs


squish' :: [[a]] -> [a]
squish' = concat


squish'' :: [[a]] -> [a]
squish'' arr = go arr []
    where
        append :: [a] -> [a] -> [a]
        append [] ys = ys
        append xs [] = xs
        append (x:xs) ys = x : (append xs ys)

        go :: [[a]] -> [a] -> [a]
        go [] result = result
        go (x:xs) result = append x $ go xs result


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap fn (x:xs) = (fn x) ++ (squishMap fn xs)


squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy fn (x1:x2:xs)
    | fn x1 x2 == GT = myMaximumBy fn (x1:xs)
    | otherwise = myMaximumBy fn (x2:xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (x:[]) = x
myMinimumBy fn (x1:x2:xs)
    | fn x1 x2 == LT = myMinimumBy fn (x1:xs)
    | otherwise = myMinimumBy fn (x2:xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
