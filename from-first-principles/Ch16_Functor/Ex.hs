{-# LANGUAGE FlexibleInstances #-}

module Ex where

import Test.QuickCheck
import Test.QuickCheck.Function


-- 1.
--
data Sum b a =
    First a 
    | Second b

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b

-- 2. 
--
data Company a c b =
    DeepBlue a c
    | Something b

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

-- 3.
--
data More b a =
    L a b a
    | R b a b deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

addOneL = fmap (+1) (L 1 2 3)
addOneR = fmap (+1) (R 1 2 3)

-- Write Functor instance
--
-- 1.
--
data Quant a b =
    Finance
    | Desk a
    | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
    fmap f Finance = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

-- 2.
--
data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
    fmap f (K a) = K a

k1 :: K Int String
k1 = K 1
--k1' = fmap (+1) k1

-- 3.
--
newtype Flip f a b =
    Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K b)) = Flip (K $ f b)

kf :: Flip K String Int
kf = Flip k1
kf' = fmap (+1) kf

-- 4.
--
data EvilGoateeConst a b =
    GoatyConst b
    deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

-- 5.
--
data LiftItOut f a =
    LiftItOut (f a)
    deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
    fmap f (LiftItOut fn) = LiftItOut $ fmap f fn

-- 6.
--
data Parappa f g a =
    DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fn gn) = DaWrappa (fmap f fn) (fmap f gn)

-- 7.
--
data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fn gn) = IgnoringSomething fn (fmap f gn)

-- 8.
--
data Notorious g o a t =
    Notorious (g o) (g a) (g t)
    deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9.
--
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- 10.
--
data GoatLord a =
    NoGoat
    | OneGoat a
    | MoreGoat (GoatLord a)
               (GoatLord a)
               (GoatLord a)

instance Functor GoatLord where
    fmap f NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat $ f a
    fmap f (MoreGoat a1 a2 a3) = 
        MoreGoat (fmap f a1) (fmap f a2) (fmap f a3)

-- 11.
--
data TalkToMe a =
    Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where
    fmap f Halt = Halt
    fmap f (Print s a) = Print s $ f a
    fmap f (Read sta) = Read (f . sta)

 
