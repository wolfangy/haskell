module CountEntriesT where

import System.Directory
import System.FilePath
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Writer

import CountEntries

countEntriesT :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntriesT path = do
    -- liftIO :: MonadIO m => IO a -> m a
    -- listMyDirectory :: FilePath -> IO [String]
    -- liftIO . listMyDirectory :: MonadIO m => FilePath -> WriterT [(FilePath, Int)] IO [String]
    contents <- liftIO . listMyDirectory $ path
    -- tell :: Monad m => w -> WriterT w m ()
    -- tell [(path, length contents)] :: WriterT [(FilePath, Int)] IO ()
    tell [(path, length contents)]

    -- forM_ :: (Traversable t, Monad m) => t a -> (a -> m b) -> m ()
    forM_ contents $ \name -> do
        let newName = path </> name
        -- liftIO . doesDirectoryExist :: WriterT [(FilePath, Int)] IO Bool
        isDir <- liftIO . doesDirectoryExist $ newName
        -- when :: Applicative f => Bool -> f () -> f ()
        when isDir $ countEntriesT newName
