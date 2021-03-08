module Ex where

import Data.Semigroup
import Data.Monoid
import Test.QuickCheck

-- Semigroup
--

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- 1. 
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial


type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2.
--
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where 
    (Identity a) <> Identity (b) = Identity $ a <> b

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        v <- arbitrary
        return $ Identity v

type IdentityOneAssoc a = Identity a -> Identity a -> Identity a -> Bool

-- 3.
--
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Two x y

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

-- 4.
--
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three x y z) <> (Three x' y' z') = Three (x<>x') (y<>y') (z<>z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

type ThreeAssoc a b c = Three a b c
                                -> Three a b c
                                -> Three a b c
                                -> Bool

-- 6.
--
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj a) <> (BoolConj b) = BoolConj $ a && b

instance Arbitrary BoolConj where
    arbitrary = frequency [(1, return $ BoolConj True), 
                            (1, return $ BoolConj False)]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7.
--
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj a) <> (BoolDisj b) = BoolDisj $ a || b

instance Arbitrary BoolDisj where
    arbitrary = do
        v <- choose (False, True)
        return $ BoolDisj v

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8.
--

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Fst a) <> (Fst b) = Fst b
    (Fst a) <> (Snd b) = Snd b
    (Snd a) <> (Fst b) = Snd a
    (Snd a) <> (Snd b) = Snd a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, return $ Fst a), (1, return $ Snd b)] 

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

-- 9.
--
newtype Combine a b =
    Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
    (Combine { unCombine = f }) <> (Combine { unCombine = g }) =
        Combine { unCombine = (\x -> (f x) <> (g x)) }

instance (Monoid b) => Monoid (Combine a b) where
    mempty = Combine (\x -> mempty)

instance Show (Combine a b) where
    show _ = "Combine a b"

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = fmap Combine arbitrary

combineSemigroupAssoc :: (Eq b, Show b, Semigroup b) => a 
                                                        -> Combine a b
                                                        -> Combine a b
                                                        -> Combine a b 
                                                        -> Bool 
combineSemigroupAssoc x (Combine f) (Combine g) (Combine h) =
    (f x <> (g x <> h x)) == ((f x <> g x) <> h x)

type CombineSemigroupAssoc a b = a
                                -> Combine a b
                                -> Combine a b
                                -> Combine a b
                                -> Bool

fCombine = Combine $ \n -> Sum (n + 1)
gCombine = Combine $ \n -> Sum (n - 1)

result_f_g_0 = unCombine (fCombine <> gCombine) $ 0
result_f_g_1 = unCombine (fCombine <> gCombine) $ 1
result_f_f_1 = unCombine (fCombine <> fCombine) $ 1
result_g_f_1 = unCombine (gCombine <> fCombine) $ 1

-- 10.
--

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp $ f.g

instance Show (Comp a) where
    show _ = "Comp a"

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Comp a) where
    arbitrary = fmap Comp arbitrary

compSemigroupAssoc :: (Eq a, Show a, Semigroup a) => a
                                                    -> Comp a
                                                    -> Comp a
                                                    -> Comp a
                                                    -> Bool
compSemigroupAssoc x (Comp f) (Comp g) (Comp h) =
    (f x <> (g x <> h x)) == ((f x <> g x) <> h x)

type CompSemigroupAssoc a = a -> Comp a -> Comp a -> Comp a -> Bool

fComp = Comp $ \(Sum n) -> Sum (n + 1)
gComp = Comp $ \(Sum n) -> Sum (n - 1)

result_f_g_0' = unComp (fComp <> gComp) $ 0
result_f_g_1' = unComp (fComp <> gComp) $ 1
result_f_f_1' = unComp (fComp <> fComp) $ 1
result_g_f_1' = unComp (gComp <> fComp) $ 1

runSemigroupAssoc :: IO ()
runSemigroupAssoc = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: IdentityOneAssoc String)
    quickCheck (semigroupAssoc :: TwoAssoc String [Int])
    quickCheck (semigroupAssoc :: ThreeAssoc String [Int] (Sum Integer))
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: OrAssoc String [Int])
    quickCheck (combineSemigroupAssoc :: CombineSemigroupAssoc (Sum Int) (Sum Int))
    quickCheck (compSemigroupAssoc :: CompSemigroupAssoc String) 



newtype Mem s a =
    Mem { runMem :: s -> (a, s)}

instance Semigroup a => Semigroup (Mem s a) where
    (Mem { runMem = xFn }) <> (Mem { runMem = yFn}) = 
        Mem { runMem = yFn . snd . xFn }

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem { runMem  = \v -> (mempty, v) }

f' = Mem $ \s -> ("hi", s + 1)

main = do
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0
