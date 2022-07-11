module StateTest where

import Data.Foldable
import Control.Monad.State

type StateIntT = StateT Integer IO

addItem :: Integer -> State Integer ()
addItem n = do
    s <- get
    put (s + n)

addItem' :: Integer -> State Integer ()
addItem' n = modify' (+n)

sumList :: [Integer] -> State Integer ()
sumList xs = 
    traverse_ addItem xs

addItemT :: Integer -> StateIntT ()
addItemT n = do
    s <- get
    lift $ putStrLn $ "Get value: " <> show s
    put (s + n)
    lift $ putStrLn $ "Update value to: " <> show (s + n)

data Config = Config {
        verbose :: Bool
    }

type ConfigT = StateT Config IO

work :: ConfigT ()
work = do
    doSomething

doSomething :: ConfigT ()
doSomething = do
    doSomethingSpecial

doSomethingSpecial :: ConfigT ()
doSomethingSpecial = do
    verb <- gets verbose
    lift $ putStrLn $ "Load Config Verbose: " <> show verb
    when verb $ do
        modify' silent
        newVerb <- gets verbose
        lift $ putStrLn $ "Update Config Verbose: " <> show newVerb

silent :: Config -> Config
silent Config{verbose=prev} = Config { verbose = not prev}
