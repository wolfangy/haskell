module MonoidTest where

import Data.Monoid
import Data.Semigroup
import Control.Monad
import Optional
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools),
                             (1, return Twoo)]

instance Semigroup Bull where
    _ <> _ = Fools

instance Monoid Bull where
    mempty = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool


-- Exercise
--

newtype First' a = First' { getFirst' :: Optional a} deriving (Eq, Show)

instance (Semigroup a) => Semigroup (First' a) where
    (First' { getFirst' = x }) <> (First' { getFirst' = y}) =
        First' { getFirst' = x <> y }

instance (Monoid a) => Monoid (First' a) where
    mempty = First' { getFirst' = Nada }

instance (Arbitrary a) => Arbitrary (First' a) where
    arbitrary = frequency[
        (1, return First' { getFirst' = Nada })
        , (3, do
            val <- arbitrary
            return First' { getFirst' = Only val})]

firstMappend :: (Monoid a) => First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FirstId = First' String -> Bool

main :: IO ()
main = do
    quickCheck (monoidAssoc :: BullMappend)
    quickCheck (monoidLeftIdentity :: Bull -> Bool)
    quickCheck (monoidRightIdentity :: Bull -> Bool)
    quickCheck (monoidAssoc :: FirstMappend)

    quickCheck (monoidLeftIdentity :: FirstId)
    quickCheck (monoidRightIdentity :: FirstId)
