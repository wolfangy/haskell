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
    Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
    fmap f (Constant a) = Constant a

instance (Semigroup a) =>  Semigroup (Constant a b) where
    (Constant a) <> (Constant a') = Constant (a <> a')

instance Foldable (Constant a) where
    foldMap _ (Constant g) = mempty
    
instance Traversable (Constant a) where
    traverse _ (Constant g) = pure (Constant g)

data Optional a =
    Nada | Yap a deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yap a) = Yap $ f a

instance Applicative Optional where
    pure = Yap
    Nada <*> _ = Nada
    _ <*> Nada = Nada
    (Yap f) <*> (Yap a) = Yap $ f a

instance Foldable Optional where
    foldMap f Nada = mempty
    foldMap f (Yap a) = f a

    foldr f b Nada = b
    foldr f b (Yap a) = f a b

instance Traversable (Optional) where
    traverse f Nada = pure Nada
    traverse f (Yap a) = Yap <$> f a




