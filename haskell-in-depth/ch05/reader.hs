import Control.Monad.Reader
import Control.Monad.Identity
import Data.Foldable (traverse_)

data Config = Config {
        verbose :: Bool
    }

-- type ConfigM = Reader Config
type ConfigM = ReaderT Config Identity

type ConfigT = ReaderT Config IO

getConfiguration :: IO Config
getConfiguration = pure Config {
        verbose = True
    }

work :: ConfigM ()
work = do
    doSomething

work' :: ConfigT ()
work' = do
    doSomething'

doSomething :: ConfigM ()
doSomething = do
    doSomethingSpecial

doSomething' :: ConfigT ()
doSomething' = do
    doSomethingSpecial'

doSomethingSpecial :: ConfigM ()
doSomethingSpecial = do
    verb <- asks verbose
    when verb beVerbose

doSomethingSpecial' :: ConfigT ()
doSomethingSpecial' = do
    verb <- asks verbose
    lift . print $ verb

beVerbose :: ConfigM ()
beVerbose = pure ()

silent :: Config -> Config
silent config = config { verbose = False}

doSomethingSpecialSilently :: ConfigM ()
doSomethingSpecialSilently = local silent doSomethingSpecial

showInt :: Reader Int (IO ())
showInt = reader $ \i -> putStrLn $ show i

showIntT :: ReaderT Int IO ()
showIntT = do
    i <- ask
    let
        incI = i + 1
        str = show incI
    lift $ putStrLn str

main :: IO ()
main = do
    config <- getConfiguration
    let result = runReader work config
    print result
    let 
        w = local silent work
        r = runReader w config
    print r