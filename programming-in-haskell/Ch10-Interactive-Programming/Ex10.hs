{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
module Ex10 where

import Nim(putRow, Board)
import Hangman(getCh)

-- 1. 

putStr':: String -> IO ()
putStr' xs = sequence_ [ putChar x | x <- xs]

-- 2.

-- 3.
putBoard :: Board -> IO ()
putBoard board = sequence_ [putRow row num | (row, num) <- zip [1..] board]

-- 4.

adder :: IO ()
adder = do
    putStr' "How many numbers? "
    ch <- getLine
    total <- askNumber (toInt ch) 0
    putStr' "The total is: "
    print total
    putStrLn ""

toInt :: String -> Int
toInt str = read str :: Int

askNumber :: Int -> Int -> IO Int
askNumber 0 total = return total
askNumber left total = do
    putStr "> "
    num <- getLine
    askNumber (left - 1) (total + toInt num)

-- 5.

adder' :: IO ()
adder' = do
    putStr "How many numbers: "
    count <- getLine
    total <- sum <$> sequence [ askNumber' | _ <- [1..(toInt count)]]
    print $ "The total is: " ++ show total

askNumber' :: IO Int
askNumber' = do
    putStr ": "
    toInt <$> getLine

-- 6.

readLine :: IO String
readLine = readLineWithBuffer []
    where
        readLineWithBuffer xs = do
            c <- getCh
            case (c, xs) of
                ('\DEL', []) -> readLineWithBuffer []
                ('\DEL', xs) -> readLineWithBuffer $ init xs
                ('\n', xs) -> return xs
                otherwise -> readLineWithBuffer $ xs ++ [c]