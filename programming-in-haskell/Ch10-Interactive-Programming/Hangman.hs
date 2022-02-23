module Hangman where

import System.IO

sgeLine :: IO String
sgeLine = do
        x <- getCh
        if x == '\n' then
            do
                putChar x
                return []
        else
            do
                putChar '-'
                xs <- sgeLine
                return (x:xs)

getCh :: IO Char
getCh = do
        hSetEcho stdin False
        x <- getChar
        hSetEcho stdin True
        return x

play :: String -> IO ()
play word = do
            putStr "? "
            guess <- getLine
            if guess == word then putStrLn "You got it!!"
            else putStrLn (match word guess) >> play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-'| x <- xs]

main :: IO ()
main = do
        putStrLn "Think of a word: "
        word <- sgeLine
        putStrLn "Try to guess it: "
        play word
