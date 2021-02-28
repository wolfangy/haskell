module Main where 

-- To use QuickCheck in a command such as `stack ghci`
-- by adding the `--package` option
-- e.g.
-- stack ghci --package QuickCheck -- trivial.hs
--
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

-- Use the `choose` and `elements` from QuickCheck as generator of values
--


data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen

main :: IO ()
main = do
    sample trivialGen
