module Chapter6 where

-- Eq Instances
--
data Identity a = Identity' a deriving (Show)
instance Eq a => Eq (Identity a) where
    (Identity' v) == (Identity' v') = (v Prelude.== v')


data TisAnInteger = TisAn Integer deriving (Show)
instance Eq (TisAnInteger) where
    (TisAn i) == (TisAn i') = (i Prelude.== i')


data TwoIntegers = Two Integer Integer deriving (Show)
instance Eq (TwoIntegers) where
    (Two a b) == (Two a' b') = (a Prelude.== a') && (b Prelude.== b')

data StringOrInt = TisAnInt Int | TisAString StringOrInt
instance Eq (StringOrInt) where
    (TisAnInt i) == (TisAnInt i') = i Prelude.== i'
    (TisAString s) == (TisAString s') = s Prelude.== s'
    (==) (TisAnInt _) (TisAString _) = False
    (==) (TisAString _) (TisAnInt _) = False

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair i j) (Pair i' j') = i Prelude.== i' && j Prelude.== j'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = a Prelude.== a' && b Prelude.== b'

data Which a = ThisOne a | ThatOne a
instance (Eq a) => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a Prelude.== a'
    (==) (ThatOne a) (ThatOne a') = a Prelude.== a'
    (==) (ThisOne _) _ = False
    (==) (ThatOne _) _ = False

data EitherOr a b = Hello a | Goodby b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = (Prelude.==) a a'
    (==) (Goodby a) (Goodby a') = (Prelude.==) a a'
    (==) (Hello _) _ = False
    (==) (Goodby _) _ = False

-- Num
--
-- Fraction

divideThenAdd :: Fractional a => a -> a -> a
divideThenAdd x y = (x / y) + 1 

