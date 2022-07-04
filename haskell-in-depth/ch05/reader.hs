import Control.Monad.Reader
import Data.Foldable (traverse_)

data Config = Config {
        verbose :: Bool
    }

type ConfigM = Reader Config

getConfiguration :: IO Config
getConfiguration = pure Config {
        verbose = True
    }

work :: ConfigM ()
work = do
    doSomething

doSomething :: ConfigM ()
doSomething = do
    doSomethingSpecial

doSomethingSpecial :: ConfigM ()
doSomethingSpecial = do
    verb <- asks verbose
    when verb beVerbose

beVerbose :: ConfigM ()
beVerbose = pure ()

silent :: Config -> Config
silent config = config { verbose = False}

doSomethingSpecialSilently :: ConfigM ()
doSomethingSpecialSilently = local silent doSomethingSpecial

showInt :: Reader Int (IO ())
showInt = reader $ \i -> putStrLn $ show i

main :: IO ()
main = do
    config <- getConfiguration
    let result = runReader work config
    print result
    let 
        w = local silent work
        r = runReader w config
    print r