module UglyStack where

import System.Directory
import System.FilePath
import Control.Monad (when, forM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

data AppConfig = AppConfig {
        cfgMaxDepth :: Int
        } deriving (Show)

data AppState = AppState {
        stDeepestReaded :: Int
        } deriving (Show)

type App = ReaderT AppConfig (StateT AppState IO)

runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth = 
    let config = AppConfig maxDepth
        state = AppState 0
    in runStateT (runReaderT k config) state

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount  curDepth path = do
    contents <- liftIO . listDirectory $ path
    cfg <- ask
    -- forM :: [String] -> (String -> App [(FilePath, Int)]) -> App [[(FilePath, Int)]]
    rest <- forM contents $
        \name -> do
            let newPath = path </> name
            isDir <- liftIO $ doesDirectoryExist newPath
            if isDir && curDepth < cfgMaxDepth cfg
            then do
                let newDepth = curDepth + 1
                st <- lift get
                when (stDeepestReaded st < newDepth) $ lift $ put st { stDeepestReaded = newDepth }
                constrainedCount newDepth newPath
            else return []
    return $ (path, length contents) : concat rest

    -- ReaderT r (StateT s m) Int
    --  <-
