
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
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

-- newtype AccApp a = AccApp { 
--         runAccApp ::ReaderT Int (StateT [Int] IO) a 
--     }

type AccApp = ReaderT Int (StateT [Int] IO)

runAppBase :: AccApp a -> Int -> IO a
runAppBase app arg = evalStateT (runReaderT app arg) []

takeThenAcc :: AccApp ()
takeThenAcc = do
    input <- ask
    (liftIO . putStr) $ "reader input: " <> (show input) <> " >> "
    (lift . modify') $ (++ [input])
    all <- lift get
    (lift . lift . putStr) $ "state updated to: " <> (show all) <> " >> "
    (liftIO . putStrLn) $ "sum: " <> ((show . compute) $ all)
    where
        compute :: [Int] -> Int
        compute = foldr (+) 0

accTo :: Int -> AccApp ()
accTo n = do
    (liftIO . putStr) $ "intake: " <> (show n) <> " >> "
    (lift . modify') $ (n:)
    xs <- lift get
    (liftIO . putStrLn) $ "status updated to: " <> (show xs)

accArrayToApp :: [Int] -> AccApp () -> AccApp ()
accArrayToApp arr app = traverse_ (\r -> local (const r) app) arr

acc_1_10 :: AccApp ()
acc_1_10 = accArrayToApp [1..10] takeThenAcc

acc_1_10' :: AccApp ()
acc_1_10' = traverse_ accTo [1..10]