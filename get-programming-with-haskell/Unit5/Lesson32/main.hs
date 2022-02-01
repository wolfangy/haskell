import Control.Monad
import Data.Char

squareOdds :: Int -> [Int]
squareOdds n = [val |  val <- [1..n], odd val]

valAndSquare :: [(Int, Int)]
valAndSquare = do
    v <- [1..10]
    return (v, 2^v)

-- gard function
evenGuard :: Int -> [Int]
evenGuard n = do
    val <- [1..n]
    guard (even val)
    return val

filter' :: (a -> Bool) -> [a] -> [a]
filter' fn m = do
    v <- m
    guard (fn v)
    return v 

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = [(powerOfTwo, powerOfThree) | 
    value <- [1..n], 
    let powerOfTwo = 2^value, 
    let powerOfThree = 3^value]

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = [(evenValue, oddValue) | evenValue <- [2, 4 .. n], oddValue <- [1, 3..n]]

capitalize :: String -> String
capitalize (x:xs) = toUpper(x) : xs

capitalizeMrNames :: [String]
capitalizeMrNames = [ "Mr. " ++ capitalize(name) | name <- ["brown", "blue", "pink", "orange"] ]
capitalizeMrNames' = ["Mr. " ++ capVal | val <- ["brown", "blue", "pink", "orange"],
    let capVal = (\(x:xs) -> (toUpper x):xs) val]


-- Q32.1
data Months = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

calendarDates :: [[Int]]
calendarDates = [ date | month <- [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec],
    let date = (\m -> case m of
            Jan -> [1..31]
            Feb -> [1..28]
            Mar -> [1..31]
            Apr -> [1..30]
            May -> [1..31]
            Jun -> [1..30]
            Jul -> [1..31]
            Aug -> [1..31]
            Sep -> [1..30]
            Oct -> [1..31]
            Nov -> [1..30]
            Dec -> [1..31]) month ];
    

monthEnds :: [Int]
monthEnds = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

dates :: [Int] -> [Int]
dates ends = [date| end <- ends, date <- [1 ..end ]]

-- Q32.2

calendarDates' :: [[Int]]
calendarDates' = do
    month <- [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]
    let dates = calDates(month)
    return dates
    where
        calDates m = case m of
            Jan -> [1..31]
            Feb -> [1..28]
            Mar -> [1..31]
            Apr -> [1..30]
            May -> [1..31]
            Jun -> [1..30]
            Jul -> [1..31]
            Aug -> [1..31]
            Sep -> [1..30]
            Oct -> [1..31]
            Nov -> [1..30]
            Dec -> [1..31]


dates' :: [Int] -> [Int]
dates' ends = do
    end <- ends
    date <- [1..end]
    return date