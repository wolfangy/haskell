{-# LANGUAGE DeriveGeneric #-}
module Main where 

-- To use QuickCheck in a command such as `stack ghci`
-- by adding the `--package` option
-- e.g.
-- stack ghci --package QuickCheck -- trivial.hs
--
import GHC.Generics
import Test.QuickCheck

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]

oneThroughThree' :: Gen Int
oneThroughThree' = elements [1,2,2,2,3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]


genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a , Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    x <- arbitrary
    elements [Nothing, Just x]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [ (1, return Nothing), (3, return $ Just a)]

-- Use the 
-- `choose` for Tuple
-- `elements` for list
-- `frequency` :: [(Int, Gen a)] -> Gen a

-- from QuickCheck as generator of values
--


data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen


data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    a <- arbitrary
    return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

-- Arbitrary Produects

data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = pairGen

-- Greater than the sum of its parts

data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a, return $ Second b]

-- `oneof` function will create a Gen a from a list of Gen a by giving each value
-- an equal probability.

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

genMaybeInt :: Gen (Maybe Int)
genMaybeInt = genMaybe'

-- CoArbitrary
-- `coarbitrary` :: CoArbitrary a => a -> Gen b -> Gen b
--

data Bool' = True' | False' deriving (Generic)

instance CoArbitrary Bool'

trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary

main :: IO ()
main = do
    sample trivialGen
    sample identityGenInt
