import Control.Monad

main :: IO ()
main = do
    forM_ (rangeUser users) putStrLn
    putStrLn ""
    forM_ (rangeUser' users) putStrLn
    where
        users = [1, 5, 10, 15, 20, 25]

rangeUser = snd . rangeUserImpl . reverse

rangeUserImpl :: [Int] -> (Int, [String])
rangeUserImpl [] = error "At least one element"
rangeUserImpl [x] = (x, [show x])
rangeUserImpl (x:xs) =
    let
        (prev, output) = rangeUserImpl xs
    in (x, output ++ [show (prev + 1) ++ ".." ++ show x])

rangeUser' :: [Int] -> [String]
rangeUser' xs = [ if y == -1 then show x else show (x + 1) ++ ".." ++ show y | 
                (x, y) <- (head xs, negate 1) : zip xs (tail xs)]