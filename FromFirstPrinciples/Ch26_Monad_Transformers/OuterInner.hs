module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader


embedded :: MaybeT
            (ExceptT String
                     (ReaderT () IO))
            Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

val = const . Right . Just $ 1

sameResult = (const . Right . Just $ 1) ()

-- base monad : what is structurally outermost
type MyType a = IO [Maybe a]
-- the base monad is IO

embedded' :: MaybeT
            (ExceptT String
                     (ReaderT () IO))
            Int
embedded' = MaybeT $ ExceptT $ ReaderT $ (return .) (const (Right (Just 1))) 
