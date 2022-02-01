module Addition where

import Test.Hspec
import Test.QuickCheck


sayHello :: IO ()
sayHello = putStrLn "hello"

devidedBy :: Integral a => a -> a -> (a, a)
devidedBy num denom = go num denom 0
    where go n d count 
                | n < d     = (count, n)
                | otherwise = go (n - d) d (count + 1)

multiply :: (Eq a, Num a) => a -> a -> a
multiply a b = go a b 0
    where go x y acc
                | y == 0    = acc
                | otherwise = go x (y - 1) (acc + x)

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]


genChar :: Gen Char
genChar = elements ['a'..'z']


genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a,b)

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThree = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a,b,c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [ (1, return Nothing)
              , (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

prop_additionNotGreater :: Int -> Bool
prop_additionNotGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

failQc :: IO ()
failQc = quickCheck prop_additionNotGreater

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True

        it "2 + 2 is equal to 4" $ do
            (2 + 2) > 2 `shouldBe` True 

        it "15 devided by 5 is" $ do
            devidedBy 15 3 `shouldBe` (5, 0)

        it "22 devided by 5 is" $ do
            devidedBy 22 5 `shouldBe` (4, 2)

        it "5 multiply by 5 is" $ do
            multiply 5 5 `shouldBe` 25

        it "12 multiply 12 is" $ do
            multiply 12 12 `shouldBe` 144

        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
