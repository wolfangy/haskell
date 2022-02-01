module Main where

import Control.Applicative (liftA2)

boop = (*2)
doop = (+10) 

bip :: Integer -> Integer
bip = boop . doop


bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop



newtype Reader r a =
    Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ f . ra

-- Ask
--
ask :: Reader a a
ask = Reader id

instance Applicative (Reader r) where
    pure = Reader . const
    (Reader f) <*> (Reader r) = Reader (\x -> f x $ r x)

newtype HumanName =
    HumanName String
    deriving (Eq, Show)

newtype DogName =
    DogName String
    deriving (Eq, Show)

newtype Address =
    Address String
    deriving (Eq, Show)

data Person =
    Person {
        humanName :: HumanName
        , dogName :: DogName
        , address :: Address
        } deriving (Eq, Show)

data Dog =
    Dog {
        dogsName :: DogName
        , dogsAddress :: Address
        } deriving (Eq, Show)

pers :: Person
pers = 
    Person (HumanName "Big Bird")
            (DogName "Barkley")
            (Address "Sesame Street")

chris :: Person
chris = 
    Person (HumanName "Chris Allen")
            (DogName "Papu")
            (Address "Austin")

getDog :: Person -> Dog
getDog p = 
    Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR =
    Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

-- Ex
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

-- Monad
--

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

-- (>>=) :: Monad m => m    a  -> (a -> (m   b)) ->  m    b
fooBind  ::           (r -> a) -> (a -> r -> b)  -> (r -> b)
fooBind m k = \r -> k (m r) r

frooty'' :: Num a => [a] -> ([a], Int)
frooty'' = bar <*> foo

-- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
-- (=<<) :: (a -> r -> b) -> (r -> a) -> (r -> b)
-- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)

getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy

instance Monad (Reader r) where
    return = pure

    (Reader ra) >>= aRb = Reader (\x -> (runReader (aRb (ra x)) x))

getDogRM' :: Reader Person Dog
getDogRM' = undefined

main :: IO ()
main = undefined
