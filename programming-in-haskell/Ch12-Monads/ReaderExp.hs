module ReaderExp where

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map

-- Reader Exp 1:

type Bindings = Map String Int

isCountCorrect :: Bindings -> Bool
isCountCorrect bindings = runReader calcIsCountCorrect bindings

calcIsCountCorrect :: Reader Bindings Bool
calcIsCountCorrect = do
    count <- asks (lookupVar "count")
    bindings <- ask
    return (count == (Map.size bindings))

lookupVar :: String -> Bindings -> Int
lookupVar name bindings = maybe 0 id (Map.lookup name bindings)

sampleBindings :: Bindings
sampleBindings = Map.fromList [("count", 3), ("1", 1), ("b", 2)]

test1 :: IO ()
test1 = do
     putStr $ "Count is correct for bindings " ++ (show sampleBindings) ++ ": "
     putStrLn $ show (isCountCorrect sampleBindings)

-- Reader Exp 2:

calculateContentLen :: Reader String Int
calculateContentLen = do
    content <- ask
    return (length content)

calculateModifiedContentLen :: Reader String Int
calculateModifiedContentLen = local ("Prefix" ++) calculateContentLen

test2 :: IO()
test2 = do
    let s = "12345"
    let modifiedLen = runReader calculateModifiedContentLen s
    let len = runReader calculateContentLen s
    putStrLn $ "Modified 's' length: " ++ (show modifiedLen)
    putStrLn $ "Original 's' length: " ++ (show len)

test2' :: Reader String (IO())
test2' = do
    len    <- calculateContentLen
    modLen <- calculateModifiedContentLen
    return $ putStrLn (show len) >> putStrLn (show modLen)

playReader :: Reader Int String
playReader = do
    v <- ask
    return $ "The value is: " ++ show v
