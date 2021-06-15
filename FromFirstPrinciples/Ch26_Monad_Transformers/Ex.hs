module Ex where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe

-- 1
--
rDec :: Num a => Reader a a
rDec = reader (subtract 1)

decBy = runReader rDec
decByOne = decBy 1

-- 3
--
rShow :: Show a => ReaderT a Identity String
rShow = reader show

show' :: Show a => a -> Identity String
show' a = runReaderT rShow a


-- 5
-- 
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $
    \r -> do 
        print $ "Hi: " ++ show r
        return (r + 1)

-- 6
--
sPrintIncAccum :: (Num a, Show a) => StateT a IO String 
sPrintIncAccum = StateT $
    \s -> do
        let str = show s
        print $ "Hi: "  ++ str
        return (str, s + 1)

-- Fix the code

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $ do
    v <- getLine
    guard (isValid v) 
    return $ Just $ v

doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- runMaybeT maybeExcite
    case excite of
        Nothing -> putStrLn "MOAR EXCITE"
        Just e  ->
            putStrLn
            ("Good, was very excite: " ++ e)
