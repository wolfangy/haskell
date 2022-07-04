{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.Tuple (swap)

newtype MaybeT m a=
    MaybeT { runMaybeT :: m (Maybe a)}

instance (Functor m) => Functor (MaybeT m) where
    fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
    pure = MaybeT . pure . pure

    (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
    MaybeT f <*> MaybeT ma = MaybeT $ (<*>) <$> f <*> ma
    --                                  (1)  (2)(3)(4)
    -- (1) (<*>) :: f (a -> b) ->  f a -> f b
    -- (2) (<$>) ::     (a     ->      b)          ->      a     -> b
    -- (1) (<*>) :: f (a -> b) -> (f a -> f b)
    -- (3)   f   :: m Maybe (a -> b)
    -- :t (<*>) <$> f :: [  g    (a -> b) -> (    g a ->     g b)) -> f   g   (a -> b) -> f (  g   a ->   g   b)
    -- :t (<*>) <$> f :: [(Maybe (a -> b) -> (Maybe a -> Maybe b)] -> m Maybe (a -> b) -> m (Maybe a -> Maybe b)

instance (Monad m) => Monad (MaybeT m) where
    return = pure

    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (MaybeT ma) >>= f = MaybeT $ do
        a <- ma
        case a of
            Nothing -> return Nothing
            Just v  -> runMaybeT (f v)

-- EitherT
--
--
newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
    pure = EitherT . pure . pure

    (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
    EitherT emf <*> EitherT ema = EitherT $ (<*>) <$> emf <*> ema

instance Monad m => Monad (EitherT e m) where
    return = pure

    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    EitherT mea >>= f = EitherT $ do
        ea <- mea
        case ea of
            Left  e -> return $ Left e
            Right a -> runEitherT (f a)

swapEither :: Either e a -> Either a e
swapEither (Left e)  = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema)= EitherT $ fmap swapEither ema

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT famc fbmc (EitherT mab) = do
    ab <- mab
    case ab of
        Left  a -> famc a
        Right b -> fbmc b

-- ReaderT
--
--
newtype ReaderT r m a =
    ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap f (ReaderT rma) = ReaderT $ (fmap. fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
    pure :: f -> ReaderT r m f
    pure = ReaderT . const . pure

    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    ReaderT rmf <*> ReaderT rma = ReaderT $ (<*>) <$> rmf <*> rma

-- :t (<*>) <$> m :: Applicative m => r -> m a -> m b

instance (Monad m) => Monad (ReaderT r m) where
    return = pure

    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    ReaderT rma >>= f = ReaderT $ \r -> do
        a <- rma r
        runReaderT (f a) r

-- StateT
--
newtype StateT s m a =
    StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap f (StateT sma) = StateT $ 
        \s -> let m_as = sma s
              in fmap (swap. (fmap f) . swap) m_as


instance Monad m => Applicative (StateT s m) where
    pure f = StateT $ \s -> pure (f, s)

    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    StateT sma_b <*> StateT sma = StateT $
        \s -> do
            (a, s') <- sma s
            (f, _)  <- sma_b s
            return (f a, s)

instance Monad m => Monad (StateT s m) where
    return = pure
    
    (>>=) :: (StateT s m a) -> (a -> StateT s m b) -> StateT s m b
    (StateT sma) >>= f = StateT $
        \s -> do
            (a, _) <- sma s
            (b, _) <- (runStateT . f) a s
            return (b, s)

main :: IO ()
main = undefined
