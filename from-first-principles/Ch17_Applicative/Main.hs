module Main where

import Data.Char
import qualified Data.Map as M
import Data.Semigroup
import Data.Functor.Identity
import Data.Functor.Constant
import Control.Applicative
import Data.List (elemIndex)


liftA' :: Applicative f => (a -> b) -> f a -> f b
liftA' f fa = f <$> fa

liftB' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftB' f fa fb = f <$> fa <*> fb
liftB'' f fa fb = pure f <*> fa <*> fb

liftC' :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftC' f fa fb fc = f <$> fa <*> fb <*> fc


-- Define a Functor in terms of a provided Appliicative instance
--
fmap' f x = pure f <*> x

-- f :: a -> b
-- pure f :: f (a -> b)
-- x :: f a
-- pure f <*> x :: f b
--

-- The applicatives are monoidal functors
--
-- ($)   ::   (a -> b) ->   a ->   b
-- (<$>) ::   (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b

-- (<>) :: Semigroup f =>  f   -> f
--  $   ::                  (a -> b) ->   a ->   b
-- <$>  ::                f (a -> b) -> f a -> f b

table :: [(Int, String)]
table = [(3, "hello"), (4, "world")]

capitle :: String -> String
capitle (x:xs) = (toUpper x) : xs

capitledRecord = capitle <$> (M.lookup 3 $ M.fromList table)

f x = lookup x [(3, "hello")
              , (4, "julie")
              , (5, "kbai")]

g y = lookup y [(7, "sup?")
              , (8, "chris")
              , (9, "aloha")]

appended1 = (++) <$> f 3 <*> g 8
appended2 = liftA2 (++) (g 9) (f 4)

-- 1.
--
add :: Maybe Integer
add = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

-- 2.
--
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.
--
x :: Maybe Int
x = elemIndex 3 [1..5]

y2 :: Maybe Int
y2 = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y2


-- 4.
--
xs = [1,2,3]
ys = [4,5,6]

x4 :: Maybe Integer
x4 = lookup 3 $ zip xs ys

y4 :: Maybe Integer
y4 = lookup 2 $ zip xs ys


summed :: Maybe Integer
summed = fmap sum $ pure (,) <*> x <*> y

-- Identity
-- a way to introduce structure without changing the semantics of what you're doing
--
mkId = Identity

ar1 = [1,2,3]
ar2 = [9,9,9]

result1 = const <$> ar1 <*> ar2
result2 = const <$> (mkId ar1) <*> (mkId ar2)

newtype Identity' a = Identity' a deriving (Eq, Ord, Show)

instance Functor Identity' where
    fmap f (Identity' a) = Identity' $ f a

instance Applicative Identity' where
    pure a = Identity' a
    (<*>) (Identity' f) (Identity' a) = Identity' $ f a

-- Constant
-- This not only provides structure it also acts like the const function
--
newtype Constant' a b = Constant' { getConstant' :: a } deriving (Eq, Ord, Show)

instance Functor (Constant' a) where
    fmap f (Constant' g) = Constant' g

instance Monoid a => Applicative (Constant' a) where
    pure a = Constant' { getConstant' = mempty }
    (<*>) (Constant' f) (Constant' g) = Constant' {getConstant' = f <> g}

-- Maybe
-- When f is maybe, we're saying the function itself might not exist, because we're
-- allowing the possibility of the function to be applied being a Nothing case.
--
validateLength :: Int -> String -> Maybe String
validateLength maxLen s = 
    if (length s) > maxLen 
        then Nothing
        else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> (mkName n) <*> (mkAddress a)


-- Law 4. Interchange
--
-- u <*> pure y = pure ($ y) <*> u
-- u represent a function enbeded in some structure

mPure :: a -> Maybe a
mPure = pure

embed :: Num a => Maybe ((a -> b) -> b)
embed = mPure ($ 2)

mApply :: Maybe ((a -> b) -> b)
        -> Maybe (a -> b)
        -> Maybe b
mApply = (<*>)

myResult = pure ($ 2) `mApply` Just (+2)

main :: IO ()
main = undefined
