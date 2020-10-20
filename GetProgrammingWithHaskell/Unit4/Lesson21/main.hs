
import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = mconcat ["Hello ", name, "!"]

main :: IO ()
main = do
    putStrLn "Hello! What's your name?"
    name <- getLine
    let statement = helloPerson name
    putStrLn statement


nameData :: Map.Map Int String
nameData = Map.fromList [(1, "Adam"), (2, "Bob"), (3, "Jacky")]

-- Q21.1
main' :: Maybe String
main' = do
    n <- Map.lookup 2 nameData
    let statement = helloPerson n
    return statement


-- Q21.2

fastFib :: Integer -> Integer -> Integer -> Integer
fastFib n1 n2 1 = n1
fastFib n1 n2 2 = n2
fastFib n1 n2 counter = fastFib n2 (n1 + n2) (counter - 1)

fib' :: IO ()
fib' = do
    numStr <- getLine
    let num = read numStr :: Integer
    (putStrLn . show) (fastFib 1 1 num)
