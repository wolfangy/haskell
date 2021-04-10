module Ex where

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
    foldMap f (Identity a) = f a

instance Traversable Identity where
    traverse f (Identity a)= Identity <$> f a


newtype Constant a b = 
    Constant { getConstant :: a }

instance Functor (Constant a) where
    fmap f (Constant a) = Constant a
