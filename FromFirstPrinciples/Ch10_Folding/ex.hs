module Ch10_Ex where

import Data.Fixed

-- Warm up & review
-- 1.
-- a)
stops = "pbtdkg"
vowels = "aeiou"

--stops_vowels_stops :: String -> String -> [(Char, Char, Char)]
stops_vowels_stops' s v =  map (\x -> map (\y -> map (\z -> (x, y, z)) s) v) s

stops_vowels_stops :: String -> String -> [(Char, Char, Char)]
stops_vowels_stops s v = [(x, y, z) | x <- s, y <- v, z <- s]

-- b)
stops_vowels_stops_start_p = filter (\(x, _, _) -> x == 'p') $ stops_vowels_stops stops vowels

-- c)
nouns = ["cat", "dog", "bat", "gnu", "pig", "deer"]
verbs = ["eat", "jump over", "hunt down", "hide", "drink", "poo", "pee", "travel"]

nouns_verbs_nouns :: [String] -> [String] -> [String]
nouns_verbs_nouns ns vs = [x ++ " " ++ y ++ " " ++ z | x <- ns, y <- vs, z <- ns]

-- 2.
seekritFunc x =
    div (sum (map length (words x))) (length (words x))
-- to calculate what is average word length for an article

-- Rewrite function with fold

myAnd :: [Bool] -> Bool
myAnd = foldr (\a b -> if a == False then False else b) True

myAnd' :: [] Bool -> Bool
myAnd' = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (\a b -> if a == True then True else b) False

myOr' :: [] Bool -> Bool
myOr' = foldr (||) False


myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> if f a == True then True else b) False

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr ((||) . f)  False

myElem :: Eq a => a -> [a] -> Bool
myElem i = foldr (\a b -> i == a || b) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' i = foldr ((||) . (== i)) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myReverse' :: [a] -> [a]
myReverse' = foldl (\b a -> a : b) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myMap' :: (a -> b) -> [a] -> [b]
myMap' f = foldr (\a b -> (f a) : b) []


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

mySquish :: [[a]] -> [a]
mySquish = foldr (\a b -> a ++ b) []

mySquish' :: [[a]] -> [a]
mySquish' = foldr (++) []

mySquishMap :: (a -> [b]) -> [a] -> [b]
mySquishMap f = foldr ((++) . f) []

mySquishAgain :: [[a]] -> [a]
mySquishAgain = mySquishMap id

myMaxBy :: (a -> a -> Ordering) -> [a] -> a
myMaxBy f [] = undefined
myMaxBy f arr = foldr (\a b -> if f a b == GT then a else b) (arr !! 0) arr

myMinBy :: (a -> a -> Ordering) -> [a] -> a
myMinBy f arr = foldr (\a b -> if f a b == LT then a else b) (arr !! 0) arr

findTheNumbers :: [] Integer
findTheNumbers = [x |
                    x <- [1000..9999],
                    (even x)
                    && (even . (`div` 10) $ x)
                    && (even . (`div` 100) $ x)
                    && (even . (`div` 1000) $ x)
                    && sqrtOfInteger x]
    where
        sqrtOfInteger :: Integer -> Bool
        sqrtOfInteger num = (== 0) . (`mod'` 1) . sqrt $ (fromInteger num :: Double)
