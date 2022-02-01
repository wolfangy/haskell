module Ch10 where

import Data.Time

-- type class Foldable: abstracted out the list-specific part of folding into a
-- typeclass that lets you reuse the same folding functions for any datatype 
-- that can be folded - not just lists.

xs = map show [1..10]

yr = foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" xs

yl = foldl (\x y -> concat ["(", x, "+", y, ")"]) "0" xs


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' fn acc (x:xs) = fn x (foldr' fn acc xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' fn acc (x:xs) = foldl' fn newAcc xs
    where newAcc = fn acc x

foldlError :: (a -> b -> b) -> b -> [a] -> b
foldlError _ acc [] = acc
foldlError fn acc (x:xs) = foldlError fn (fn x acc) xs

yr' = foldr' (\x y -> concat ["(", x, " + ", y, ")"]) "0" xs
yl' = foldl' (\x y -> concat ["(", x, " + ", y, ")"]) "0" xs
ylError = foldlError (\x y -> concat ["(", x, " + ", y, ")"]) "0" xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs

u = undefined

-- take is nonstrict
takeExp = length $ take 2 $ take 4 [1, 2, u]

shouldWork = take 4 [1,2,3,4, u]

sumFirstFour :: Int
sumFirstFour = foldr (+) 0 shouldWork

-- foldr must force an initial cons cell in order to discriminate between the []
-- and the (x:xs) cases, so the first cons cell cannot be undefined
-- The first bit fo the spine must be evaluated by foldr.

val9001_1 = foldr (\_ _ -> 9001) 0 [1..5]

val9001_2 = foldr (\_ _ -> 9001) 0 [1, 2, 3, undefined]

val9001_3 = foldr (\_ _ -> 9001) 0 ([1, 2, 3] ++ undefined)

val9001_4 = foldr (\_ _ -> 9001) 0 [1, undefined]

val9001_5 = foldr (\_ _ -> 9001) 0 [undefined, undefined]


-- Traversing the rest of the spine does not occur unless the function asks for 
-- the results of having folded the rest of the list.

const_1 = foldr const 0 [1..5]

const_2 = foldr const 0 [1, undefined]

const_3 = foldr const 0 ([1, 2] ++ undefined)

-- const_4 = foldr const 0 [undefined, 2]


squareR = foldr (^) 2 [1..3]
squareScanR = scanr (^) 2 [1..3]
-- 1 ^ (2 ^ (3 ^ (2))

squareL = foldl (^) 2 [1..3]
squareScanL = scanl (^) 2 [1..3]
-- ((((2) ^ 1) ^ 2) ^ 3)

data DatabaseItem = DbString String
                    | DbNumber Integer
                    | DbData UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [
    DbData (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbNumber 9003,
    DbString "Hello, world!",
    DbNumber 9005,
    DbData (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)),
    DbNumber 9009
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate items = foldr appendTime [] items
    where
        appendTime (DbData time) arr = time : arr
        appendTime _ arr = arr

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber items = foldr appendNumber [] items
    where
        appendNumber (DbNumber i) arr = i : arr
        appendNumber _ arr = arr

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent items = undefined

sumDb :: [DatabaseItem] -> Integer
sumDb items = foldr acc 0 items
    where
        acc (DbNumber i) prev = i + prev
        acc _ prev = prev

avgDb :: [DatabaseItem] -> Double
avgDb items = avg $ foldr acc (0, 0) items
    where 
        acc :: DatabaseItem -> (Integer, Integer) -> (Integer, Integer)
        acc (DbNumber i) (result, count) = (i + result, count + 1)
        acc _ prev = prev
        avg :: (Integer, Integer) -> Double
        avg (result, count) = (fromInteger result) / (fromInteger count)




-- fib
fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

-- first 20 fibs:
first20 = take 20 fibs

-- fibs less than 100:
lessThan100 = takeWhile (< 100) fibs

-- factorials
factorials = scanl (*) 1 [2..]
factorialsN x = factorials !! x

bool :: a -> a -> Bool -> avg
bool = undefined

maybe :: b -> (a -> b) -> Maybe a -> b
maybe = undefined


either :: (a -> c) -> (b -> c) -> Either a b -> count
either = undefined
