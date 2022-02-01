module Ch8_Ex where

import Data.List (intersperse)

cattyConny :: String -> String -> String
cattyConny x y= x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"

frappe = flippy "haha"

tst1 = appedCatty "woohoo!"
tst2 = frappe "1"
tst3 = frappe (appedCatty "2")
tst4 = appedCatty (frappe "blue")
tst5 = cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
tst6 = cattyConny (flippy "Pugs" "are") "awesome"


type Numerator = Integer
type Denominator = Integer
type Quotient = Integer
type Reminder = Integer

data DevidedResult = Result (Quotient, Reminder) | DevidedByZero

dividedBy :: Numerator -> Denominator -> DevidedResult 
deidedby _ 0 = DevidedByZero
dividedBy num denom = go num denom 0
    where
        go n d count
            | n < d = Result (count, n)
            | otherwise = go (n - d) d (count + 1)


mc91 :: (Integral a) => a -> a
mc91 n
    | n > 100 = n - 10
    | n <= 100 = mc91 . mc91 $ n + 11


digitToWord :: Int -> String
digitToWord n
    | n == 0 = "Zero"
    | n == 1 = "One"
    | n == 2 = "Tow"
    | n == 3 = "Three"
    | n == 4 = "Four"
    | n == 5 = "Five"
    | n == 6 = "Six"
    | n == 7 = "Seven"
    | n == 8 = "Eight"
    | n == 9 = "Nine"
    | otherwise = "-"


digits :: Int -> [Int]
digits n = go n []
    where
        go :: Int -> [Int] -> [Int]
        go n arr
            | n < 10 = (:) n arr
            | otherwise = go (n `div` 10) ((: arr) . (`rem` 10) $ n)

wordNumber :: Int -> String
wordNumber n = concat . (intersperse "-") . (map digitToWord) . digits $ n 
