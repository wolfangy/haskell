import Control.Monad

main :: IO ()
main = do
    forM_ (rangeUserYang users) putStrLn
    putStrLn ""
    forM_ (rangeUserFrancisco users) putStrLn
    where
        users = [1, 5, 10, 15, 20, 25]

rangeUserYang :: [Int] -> [String]
rangeUserYang = snd . rangeUserImpl . reverse

rangeUserImpl :: [Int] -> (Int, [String])
rangeUserImpl [] = error "At least one element"
rangeUserImpl [x] = (x, [show x])
rangeUserImpl (x:xs) =
    let
        (prev, output) = rangeUserImpl xs
    in (x, output ++ [show (prev + 1) ++ ".." ++ show x])

rangeUserFrancisco :: [Int] -> [String]
rangeUserFrancisco xs = [ if y == -1 then show x else show (x + 1) ++ ".." ++ show y | (x, y) <- (head xs, negate 1) : zip xs (tail xs)]