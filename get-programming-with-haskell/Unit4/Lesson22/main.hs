import System.Environment
import Control.Monad

main :: IO()
main = do
    args <- getArgs
    let linesToRead = if length args > 0
        then read (head args)
        else 0 :: Int
    numbers <- replicateM linesToRead getLine
    let ints = map read numbers :: [Int]
    print "Sum goes here: "
    mapM_ putStrLn args

get4 :: IO()
get4 = do
    inputs <- mapM (\_ -> getLine) (take 3 (repeat 1))
    mapM_ putStrLn inputs

-- get4' :: IO()
myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n fn = mapM (\_ -> fn) (take n (repeat 1))

input_lazy :: IO()
input_lazy = do
    userInput <- getContents
    mapM_ print userInput

input_reverse_lazy :: IO ()
input_reverse_lazy = do
    userInput <- getContents
    let reversed = reverse userInput
    mapM_ print reversed
    where
        reverse [] = []
        reverse (x:xs) = reverse(xs) ++ [x]

toInts :: [Char] -> [Int]
toInts = (map read) . lines

sum_lazy :: IO()
sum_lazy = do
    userInput <- getContents
    let numbers = toInts userInput
    print (sum numbers)

mainSumSquare :: IO()
mainSumSquare = do
    userInput <- getContents
    let numbers = toInts userInput
    print (sum (map (^ 2) numbers))