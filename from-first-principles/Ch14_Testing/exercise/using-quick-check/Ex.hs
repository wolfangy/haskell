module Ex where

import Data.List
import Lib
import Test.QuickCheck

--1. 
-- for a function the identity property should hold

prop_halfAndBackAgain :: Rational -> Bool 
prop_halfAndBackAgain f = f == (halfIdentity f)

runHalfQc :: IO ()
runHalfQc = quickCheck prop_halfAndBackAgain

-- 2.
-- for any list you apply sort to
-- this property should hold
prop_sortListOrdered :: [Int] -> Bool
prop_sortListOrdered = listOrdered . sort

runSortListOrdered :: IO ()
runSortListOrdered = quickCheck prop_sortListOrdered


-- 3.
--

prop_intPlusAssoicative :: Int -> Int -> Int -> Bool
prop_intPlusAssoicative = plusAssociative

prop_intPlusCommutative :: Int -> Int -> Bool
prop_intPlusCommutative = plusCommutative

prop_floatPlusAssociative :: Float -> Float -> Float -> Bool
prop_floatPlusAssociative = plusAssociative

prop_floatPlusCommutative :: Float -> Float -> Bool
prop_floatPlusCommutative = plusCommutative


-- 4.
--
prop_intMultiplyAssociative :: Int -> Int -> Int -> Bool
prop_intMultiplyAssociative = multiplyAssociative


-- 5.
--

genY :: Gen Int
genY = arbitrary `suchThat` (/= 0)

prop_quotRemIdentity :: Int -> Int -> Bool
prop_quotRemIdentity  x y = (quotRemIdentity x y) == x

prop_divModIdentity :: Integer -> Integer -> Bool
prop_divModIdentity x y = divModIdentity x y == x

