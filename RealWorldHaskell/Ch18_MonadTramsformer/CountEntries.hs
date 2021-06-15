module CountEntries where

import System.Directory
import System.FilePath
import Control.Monad

listMyDirectory :: FilePath -> IO [String]
listMyDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
    -- contents :: [String]
    contents <- listMyDirectory path
    -- forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
    -- forM contents (\name -> ...) :: [[String]]
    rest <- forM contents $ \name -> do
    -- (</>) :: FilePath -> FilePath -> FilePath
        let newName = path </> name
    -- doesDirectoryExist :: FilePath -> IO Bool
    -- isDir :: Bool
        isDir <- doesDirectoryExist newName
        if isDir
                then countEntriesTrad newName
                else return []

    return $ (path, length contents) : concat rest
