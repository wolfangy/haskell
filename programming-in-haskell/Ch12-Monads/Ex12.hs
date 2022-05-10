{-# LANGUAGE GADTs #-}
module Ex12 where


-- 1. 

data Tree a where
    Leaf :: Tree a
    Node :: Tree a -> a -> Tree a -> Tree a
    deriving Show


instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)


tree = Node (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) 4 (Node Leaf 5 Leaf)

-- 2.

-- instance Functor ((->) a) where
--    fmap f g = f . g

-- 3. 

--instance Applicative ((->) a) where
    -- pure :: a -> (r -> a)
--    pure = const
    -- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
--    (<*>) f v = (\x -> f x $ v x)

-- 5.

-- instance Monad ((->) a) where
    -- return = const

    -- >>= :: m a -> (a -> m b) -> m b
    -- >>= :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
    -- f >>= g = (\x -> g (f x) )


-- 4.

newtype ZipList a where
    Z :: [a] -> ZipList a
    deriving Show

instance Functor ZipList where
    fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
    pure x = Z [x]

    (Z f) <*> (Z xs) = Z (f <*> xs)

-- 7.

-- data Expr a where
--     Val :: Expr Int
--     Var :: a -> Expr a
--     Add :: Expr a -> Expr a -> Expr a
--     deriving Show
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    fmap f (Var a) = Var $ f a
    fmap f (Val i) = Val i
    fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
    pure = Var

    _ <*> (Val x) = Val x
    Var f <*> Var v = Var $ f v
    Var f <*>  (Add l r) = Add (f <$> l) (f <$> r)
    Add f g <*> x = Add (f <*> x) (g <*> x)

-- 8.

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
    fmap g st = do
        v <- st
        return $ g v

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\ s -> (x, s))

    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do
        f <- stf
        x <- stx
        return $ f x

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')
