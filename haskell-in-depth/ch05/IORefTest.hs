module IORefTest where

import Data.IORef
import Text.Read (readMaybe)

sumNumbers :: IO Int
sumNumbers = do
        s <- newIORef 0
        go s
    where
        -- go :: IORef Int -> IO Int
        go acc = readNumber >>= processNumber acc

        -- readNumber :: IO (Maybe Int)
        readNumber = do
            putStr "Put integer number: "
            readMaybe <$> getLine

        -- processNumber :: IORef Int -> Maybe Int -> IO Int
        processNumber acc Nothing = readIORef acc
        processNumber acc (Just n) = modifyIORef' acc (+n) >> go acc

accumulate :: Int -> IO (IORef Int)
accumulate 0 = newIORef 0
accumulate n = do
    ref <- accumulate (n - 1)
    _ <- modifyIORef' ref (+n)
    return ref

printIORef :: Show a => IO (IORef a) -> IO String
printIORef ior = do
   r <- ior 
   show <$> readIORef r


main = do
    s <- sumNumbers
    putStr "Your sum is:"
    print s

