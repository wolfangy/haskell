module Ch6_Ex where

import Data.List(sort);
-- 1
--
data Person = Person Bool deriving Show

printPerson :: Person -> IO()
printPerson person = putStrLn (show person)

-- 2
--
data Mood = Blah | Woot deriving (Eq, Show)

settleDown x = if x == Woot then Blah
    else x

-- 3
--
instance Ord (Mood) where
    compare Blah Blah = EQ
    compare Woot Woot = EQ
    compare Blah Woot = LT
    compare _ _ = GT

settleDown' x = if x > Woot then Blah else x

-- 4
--
type Subject = String
type Verb = String
type Object = String

data Sentense = Sentense Subject Verb Object deriving (Eq, Show)

s1 = Sentense "dogs" "drool"
s2 = Sentense "Julie" "loves" "dogs"


-- 5
--
data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)


-- 5.1
phew = Papu (Rocks "chases") (Yeah True)


-- 5.2
truth = Papu (Rocks "chomskydoz")
            (Yeah True)

-- 5.3
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'


-- 5.4
instance Ord (Rocks) where
    compare (Rocks s) (Rocks s') = (Prelude.compare) s s'

instance Ord (Yeah) where
    compare (Yeah b) (Yeah b') = (Prelude.compare) b b'

instance Ord (Papu) where
    compare (Papu r y) (Papu r' y') = if yeah == EQ then rock else yeah
        where
            yeah = Prelude.compare y y'
            rock = Prelude.compare r r'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'


-- Match the types
--
-- 1.
-- i :: a
-- No instance for (Num a) arising from the literal `1'
i :: Num a => a
i = 1

-- 2.
--
-- f :: Num a => a
f :: Float
f = 1.0

-- 3.
--
f' :: Fractional a => a
--f' :: Float
f' = 1.0

-- 4.
--
f'' :: RealFrac a => a
--f'' :: Float
f'' = 1.0

-- 5.
--
freud :: Ord a => a -> a
--freud :: a -> a
freud x = x

-- 6.
--
freud' :: Int -> Int
--freud' :: a -> a
freud' x = x

-- 7.
--
myX = 1 :: Int

--sigmud :: a -> a
sigmud :: Int -> Int
sigmud x = myX

-- 8.
--
--sigmund' :: Num a => a -> a
sigmund' :: Int -> Int
sigmund' x = myX

-- 9. 
-- 
jung :: [Int] -> Int
--jung :: Ord a => [a] -> a
jung xs = head (sort xs)


-- 10.
--
young :: Ord a => [a] -> a
--young :: [Char] -> Char
young xs = head (sort xs)


-- 11.
--
mySort :: [Char] -> [Char]
mySort = sort

--signifier :: Ord a => [a] -> a
signifier :: [Char] -> Char
signifier xs = head (mySort xs)


-- Type-Kwon-Do Two
--
-- 1
--
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fn a b = b == (fn a)

-- 2
--
arith :: Num b => (a -> b) -> Integer -> a -> b
arith fn i a = fn a + (fromInteger i)
