module Main where

import Data.Monoid
import Data.Foldable
import Data.Functor

-- class Foldable (t:: * -> *) where
-- foldMap :: Monoid m => (a -> m) -> t a -> m
-- foldr :: (a -> b -> b) -> b -> t a -> b

foldSum = fold $ map Sum [1..5]
foldSum' = foldMap Sum [1..5]

foldProd = fold $ map Product [1..5]
foldProd' = foldMap Product [1..5]

foldStr = fold ["hello", " julie"]
foldStr' = foldr (++) "" ["hello", " julie"]
foldStr'' = foldMap id ["hello", " julie"]

foldMaybeFirst = foldMap First [Just 1, Nothing, Just 3]
foldMaybeLast = foldMap Last [Just 1, Nothing, Just 3]

-- foldMap can have a function to map
foldMap_x5 = foldMap (*5) $ map Product [1..3]
-- (Product 1 * 5) <> (Product 2 * 5) <> (Product 3 * 5)

foldr_x5 = foldr (*) 5 $ map Product [1..3]
-- ((Product 1) * ((Product 2) * ((Product 3) * 5)))

foldMap_x5' = foldMap (*5) $ map Sum [1..3]
-- Sum (1 * 5) <> Sum (2 * 5) <> Sum (3 * 5)

foldr_x5' = foldr (*) 5 $ map Sum [1..3]
-- ((Sum 1) * ((Sum 2) * ((Sum 3) * 5)))

-- If only trying to fold only contain one value,
-- Monid instance won't change the behavior
-- !! Still need to specify the Monoid to satisfy the type checker
--
--
valueWontAffect = foldMap (*5) (Just 100) :: Product Integer
-- (Product (100 * 5)) <> (Product mempty)
--
valueWontAffect' = foldMap (*5) (Just 100) :: Sum Integer

valueWontAffect'' = foldMap (*5) [100] :: Product Integer

different = foldMap (*5) Nothing :: Product Integer
different' = foldMap (*5) Nothing :: Sum Integer

-- toList :: t a -> [a]
--
listOfList = map toList [Just 1, Just 2, Just 3]
onlyList = concatMap toList [Just 1, Just 2, Just 3]
onlyList' = concatMap toList [Just 1, Just 2, Nothing]

-- null :: t a -> Bool
-- 

-- length :: t a -> Int
--
lengthArr = length [(1,2), (3,4), (5,6)]
lengthArr' = fmap length [(1,2), (3,4), (5,6)]

lengthWeid = fmap length Just [1..3]
-- fmap length Just ~ (length . Just)

--elelm :: Eq a => a -> t a -> Bool
--
willBeFalse = elem True (Left True)
willBeTrue = elem True (Right True)

-- maximum :: Ord a => t a -> a
-- minimum :: Ord a => t a -> a
--
cannotEvalEmpty = fmap maximum [Just 4, Just 3, Nothing]


-- sum :: (Foldable t, Num a) => t a -> a
-- produce :: (Foldable t, Num a) => t a -> a
--



main :: IO ()
main = undefined


