module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment

import Ch01Lib

printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
    putStrLn "All words: "
    TIO.putStrLn $ T.unlines $ map toString vocab
    where
        toString :: Entry -> T.Text
        toString (text, n) = text <> "   (" <> (T.pack . show) n  <> ")"

processTextFile :: FilePath -> IO()
processTextFile fname = do
    text <- TIO.readFile fname
    let vocab = extractVocab text
    printAllWords vocab


main :: IO ()
main = do
    args <- getArgs
    case args of
        [fname] -> processTextFile fname
        _ -> putStrLn "Usage: vocab-builder filename"
