module Main where

import Data.Monoid (mconcat)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict hiding (get)
import Control.Monad.IO.Class

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT(..))

-- liftM :: Monad m => (a -> r) -> m a -> m r
--
-- class MonadTrans t where
--      lift :: (Monad m) => m a -> t m a
-- t is a constructed monad transformer type that has an instance of MonadTrans 
-- defeind
--
-- newtype ActionT e m a =
--  ActionT {
--      runAM :: Except
--                  (ActionError e)
--                  (ReaderT ActionEnv
--                      (StateT ScottyResponse m))
--                  a
--      } deriving (Functor, Applicative)
--
--
-- instance MonadTrans (ActionT e) where
--      lift = ActionT . lift . lift . lift
--
-- instance MonadTrans (ExceptT e) where
--      lift = ExceptT . liftM Right
--
-- instance MonadTrans (ReaderT r) where
--      lift = \m -> ReaderT (const m)
--
-- instance MonadTrans (StateT s) where
--      lift = \m -> StateT (\s -> do
--                              a <- m
--                              return (a, s))
--
main = scotty 3000 $ do
    get "/:word" $ do
        beam <- param "word"
        (ActionT 
            . (ExceptT . fmap Right)
            . (\m -> ReaderT (const m))
            . (\m -> StateT(\s -> do
                                a <- m
                                return (a,s)))
            ) (putStrLn "hello")
        liftIO (putStrLn "goodbye")

        html $
            mconcat ["<h1>Scotty, ", beam, " me up! </h1>"]
