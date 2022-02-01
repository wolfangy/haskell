module Main where

import Test.QuickCheck
import Test.QuickCheck.Function

replaceWithP = const 'p'

n = Nothing

w = Just "woohoo"

ave = Just "Ave"

lms = [ave, n, w]

exp1Level0 = replaceWithP lms
-- :t replaceWithP
-- replaceWithP :: b -> Char
--

exp1Level1 = fmap replaceWithP lms
-- :t fmap replaceWithP
-- fmap replaceWithP :: Functor f => f b -> f Char
--

exp1Level2 = (fmap . fmap) replaceWithP lms
-- :t (fmap . fmap) replaceWithP
-- (fmap . fmap) replaceWithP :: (Functor f1, Functor f2) => f1 (f2 b) -> f1 (f2 Char)
--

exp1Level3 = (fmap . fmap . fmap) replaceWithP lms
-- :t (fmap . fmap . fmap) replaceWithP
-- (fmap . fmap . fmap) replaceWithP :: 
-- (Functor f1, Functor f2, Functor f3) => f1 (f2 (f3 b)) -> f1 (f2 (f3 Char))

ha = Just ["Ha",  "Ha"]
lmls = [ha, Nothing, Just []]

exp2Level2 = (fmap . fmap) replaceWithP lmls
exp2Level3 = (fmap . fmap . fmap) replaceWithP lmls

main :: IO ()
main = do
    putStr "replaceWithP: "
    print exp1Level0

    putStr "lifed replaceWithP: "
    print exp1Level1

    putStr "twice lifed replaceWithP: "
    print exp1Level2

    putStr "thrice lifed replaceWithP: "
    print exp1Level3


a = fmap (+1) $ read "[1]"::[Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi, ", "Hello "])

c = fmap (*2) (\x -> x -2)

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
d' = (fmap . fmap) ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap ((read::String -> Integer) . ("123" ++) . show) ioi
    in
        fmap (* 3) changed

data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
    fmap f (First a) = First a
    fmap f (Second b) = Second $ f b


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f


functorCompose :: (Eq (f c), Functor f) =>
                        (a -> b)
                     -> (b -> c)
                     -> f a
                     -> Bool
functorCompose f g x =
    (fmap g (fmap f x)) == (fmap (g . f) x)

fIdTest :: [Int] -> Bool
fIdTest x = functorIdentity x

composeTest = functorCompose (+1) (*2)
li x = composeTest (x :: [Int])

functorCompose' :: (Eq (f c), Functor f) =>
                    f a
                 -> Fun a b
                 -> Fun b c
                 -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

--fc' = functorCompose'

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor (Identity) where
    fmap f (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Pair x y

type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b1 <- arbitrary
        b2 <- arbitrary
        return $ Three' a b1 b2

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary(Four' a b) where
    arbitrary = do
        a1 <- arbitrary
        a2 <- arbitrary
        a3 <- arbitrary
        b <- arbitrary
        return $ Four' a1 a2 a3 b

type FourFC' = Four' Int Int -> IntToInt -> IntToInt -> Bool

runTest :: IO ()
runTest = do
    quickCheck (functorCompose' :: IntFC)

    quickCheck (functorIdentity :: Identity Int -> Bool)
    quickCheck (identityComposeTest :: Identity Int -> Bool)
    quickCheck (functorCompose' :: IdentityFC)

    quickCheck (functorIdentity :: Pair Int -> Bool)
    quickCheck (pairComposeTest :: Pair Int -> Bool)
    quickCheck (functorCompose' :: PairFC)

    quickCheck (functorIdentity :: Three Int Double Double -> Bool)
    quickCheck (threeComposeTest :: Three Int Double Integer -> Bool)


    quickCheck (functorIdentity :: Four' Int Int -> Bool)
    quickCheck (fourCompseTest :: Four' Int Int -> Bool)
    quickCheck (functorCompose' :: FourFC')
    where
        identityComposeTest = functorCompose (+1) (*2)
        pairComposeTest = functorCompose (+1) (*2)
        threeComposeTest = functorCompose (+1) (*2)
        fourCompseTest = functorCompose (+1) (*2)
