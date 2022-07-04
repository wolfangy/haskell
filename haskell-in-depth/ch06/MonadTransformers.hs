
import Control.Monad.RWS
import Data.Foldable

-- RWST Int [Int] state IO
type App state = RWST Int [Int] state IO

runApp :: App state a -> Int -> state -> IO (a, [Int])
runApp app v st = evalRWST app v st

traverseIntegers :: App s () -> App s ()
traverseIntegers app = do
    up <- ask
    traverse_ go [1..up]
    where
        go v = flip local app (\r -> r + v) 

listIntegers :: App s ()
listIntegers = do
    v <- ask
    when (v <= 10) $ do
        tell [v]
        traverseIntegers listIntegers

work :: Int -> IO ()
work start = do
    (_, list) <- runApp listIntegers start ()
    let report = show list
    putStrLn report