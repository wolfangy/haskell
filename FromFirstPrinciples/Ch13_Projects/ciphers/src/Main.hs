module Main where

import Control.Monad
import Lib (caesar, vigenere)

data Cipher = Ceasar Int String
            | Vigenr String String
            deriving (Eq, Show)

main :: IO ()
main = forever $ do
  putStrLn ("Select cipher:\n" ++ "1: Ceasar\n" ++ "2: Vigenere")
  index <- getLine
  case index of
    ('1':[]) -> applyCeasar
    ('2':[]) -> applyVigenere
    (x:xs)   -> putStrLn "Error"

applyCeasar :: IO ()
applyCeasar = do
    key <- getLine
    value <- getLine
    putStrLn $ caesar (length key) value

applyVigenere :: IO ()
applyVigenere = do
    key <- getLine
    value <- getLine
    putStrLn $ vigenere key value
