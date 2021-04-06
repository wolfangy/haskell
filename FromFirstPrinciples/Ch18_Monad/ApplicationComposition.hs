module ApplicationComposition where

import Control.Monad

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = (g a) >>= f

mcomp' :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp' f g a = join (f <$> (g a))

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
--
-- (>=>) :: (a -> m b) -> (b -> m c) -> a -> m c
-- flip (.) :: (a -> b) -> (b -> c) -> a -> c
-- 

sayHi :: String -> IO String
sayHi greeting = do
    putStrLn greeting
    getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you?"
